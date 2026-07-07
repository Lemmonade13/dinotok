#include "MKL46Z4.h"
#include <stdint.h>
#include "init.h"

void buttons_init(void) {
	// initializes buttons
    SIM->SCGC5 |= SIM_SCGC5_PORTC_MASK;
    PORTC->PCR[3]  = PORT_PCR_MUX(1) | PORT_PCR_PE_MASK | PORT_PCR_PS_MASK;
    PORTC->PCR[12] = PORT_PCR_MUX(1) | PORT_PCR_PE_MASK | PORT_PCR_PS_MASK;
    GPIOC->PDDR &= ~((1<<3) | (1<<12));
}

void uart_init(void) {
	// initializes UART to send inputs to the laptop
    SIM->SCGC4 |= SIM_SCGC4_UART0_MASK;
    SIM->SCGC5 |= SIM_SCGC5_PORTA_MASK;
    PORTA->PCR[1] = PORT_PCR_MUX(2);
    PORTA->PCR[2] = PORT_PCR_MUX(2);
    SIM->SOPT2   |= SIM_SOPT2_UART0SRC(1);
    UART0->C2     = 0;
    UART0->BDH    = 0x00;
    UART0->BDL    = 0x88;
    UART0->C4     = 0x0F;
    UART0->C2 |= UART0_C2_TE_MASK | UART0_C2_RE_MASK;
}

void uart_putchar(char c) {
	// sends input to the laptop in the form of a character
    while (!(UART0->S1 & UART0_S1_TDRE_MASK));
    UART0->D = c;
}

void i2c_init(void) {
	// Enables I2C
    SIM->SCGC4 |= SIM_SCGC4_I2C0_MASK;
    SIM->SCGC5 |= SIM_SCGC5_PORTE_MASK;
    PORTE->PCR[24] = PORT_PCR_MUX(5);
    PORTE->PCR[25] = PORT_PCR_MUX(5);
    I2C0->F  = 0x14;
    I2C0->C1 = I2C_C1_IICEN_MASK;
}

void i2c_write(uint8_t address, uint8_t reg, uint8_t value) {
	// writes value to the given register at the given I2C address
    I2C0->C1 |= I2C_C1_TX_MASK | I2C_C1_MST_MASK;
    I2C0->D = address << 1;
    for (volatile int timeout = 0; timeout < 10000 && !(I2C0->S & I2C_S_IICIF_MASK); timeout++);
    I2C0->S |= I2C_S_IICIF_MASK;
    I2C0->D = reg;
    for (volatile int timeout = 0; timeout < 10000 && !(I2C0->S & I2C_S_IICIF_MASK); timeout++);
    I2C0->S |= I2C_S_IICIF_MASK;
    I2C0->D = value;
    for (volatile int timeout = 0; timeout < 10000 && !(I2C0->S & I2C_S_IICIF_MASK); timeout++);
    I2C0->S |= I2C_S_IICIF_MASK;
    I2C0->C1 &= ~I2C_C1_MST_MASK;
}

uint8_t i2c_read(uint8_t address, uint8_t reg) {
	// reads the given register at the given I2C address
    I2C0->C1 |= I2C_C1_TX_MASK | I2C_C1_MST_MASK;
    I2C0->D = address << 1;
    for (volatile int timeout = 0; timeout < 10000 && !(I2C0->S & I2C_S_IICIF_MASK); timeout++);
    I2C0->S |= I2C_S_IICIF_MASK;
    I2C0->D = reg;
    for (volatile int timeout = 0; timeout < 10000 && !(I2C0->S & I2C_S_IICIF_MASK); timeout++);
    I2C0->S |= I2C_S_IICIF_MASK;
    I2C0->C1 |= I2C_C1_RSTA_MASK;
    I2C0->D = (address << 1) | 1;
    for (volatile int timeout = 0; timeout < 10000 && !(I2C0->S & I2C_S_IICIF_MASK); timeout++);
    I2C0->S |= I2C_S_IICIF_MASK;
    I2C0->C1 &= ~I2C_C1_TX_MASK;
    I2C0->C1 |= I2C_C1_TXAK_MASK;
    (void)I2C0->D;
    for (volatile int timeout = 0; timeout < 10000 && !(I2C0->S & I2C_S_IICIF_MASK); timeout++);
    I2C0->S |= I2C_S_IICIF_MASK;
    I2C0->C1 &= ~I2C_C1_MST_MASK;
    return I2C0->D;
}

void accel_init(void) {
	// puts accelerometer on standby
    i2c_write(MMA8451_ADDR, MMA8451_CTRL_REG1, 0x01);
}

// initialize LED (tells Player 2 when they can place an obstacle)
void led_init(void) {
    SIM->SCGC5 |= SIM_SCGC5_PORTD_MASK;

    PORTD->PCR[5] = PORT_PCR_MUX(1);
    GPIOD->PDDR |= (1<<5);

    // start with the LED off
    GPIOD->PSOR = (1<<5);
}


// checks if we receive a char from OCaml
char uart_getchar_nonblocking(void) {
    if (UART0->S1 & UART0_S1_RDRF_MASK) {
        return UART0->D;
    }
    return 0;
}
