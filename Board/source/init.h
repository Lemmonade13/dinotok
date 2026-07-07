#ifndef INIT_H
#define INIT_H

#include "MKL46Z4.h"
#include <stdint.h>

#define MMA8451_ADDR       0x1D
#define MMA8451_OUT_X_MSB  0x01
#define MMA8451_OUT_Y_MSB  0x03
#define MMA8451_OUT_Z_MSB  0x05
#define MMA8451_CTRL_REG1  0x2A

void buttons_init(void);

void uart_init(void);
void uart_putchar(char c);
char uart_getchar_nonblocking(void);

void i2c_init(void);
void i2c_write(uint8_t address, uint8_t reg, uint8_t value);
uint8_t i2c_read(uint8_t address, uint8_t reg);

void accel_init(void);

void led_init(void);

#endif
