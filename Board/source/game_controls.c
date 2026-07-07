#include "MKL46Z4.h"
#include <stdint.h>
#include "init.h"

volatile int past = 0;
volatile int past_y = 0;

int Dmain(void) {
	// initialize everything
    buttons_init();
    uart_init();
    i2c_init();
    accel_init();
    past = 0;
    past_y = 0;

    while (1) {
        // Buttons
        if (!(GPIOC->PDIR & (1<<3))) {
            uart_putchar('F'); // send F-16
            for (volatile int i = 0; i < 500000; i++);
        }
        if (!(GPIOC->PDIR & (1<<12))) {
            uart_putchar('q'); // quit game
            for (volatile int i = 0; i < 500000; i++);
        }

        // Accelerometer
        // read x and y values from accelerometer
        int8_t x = (int8_t)i2c_read(MMA8451_ADDR, MMA8451_OUT_X_MSB);
        for (volatile int i = 0; i < 10000; i++);
        int8_t y = (int8_t)i2c_read(MMA8451_ADDR, MMA8451_OUT_Y_MSB);
        for (volatile int i = 0; i < 10000; i++);

        if (x > 45 && past < 1) {
            uart_putchar('S'); // tilt back = slide
            past = 1;
            for (volatile int i = 0; i < 200000; i++);
            // loops included to make controls less erratic
        } else if (x < -45 && past > -1) {
            uart_putchar('W'); // tilt forward = jump
            past = -1;
            for (volatile int i = 0; i < 200000; i++);
        } else if ((x < 15 && past > 0) || (x > -15 && past < 0)) {
            past = 0;
        } else {
            if (y > 45 && past_y != 1) {
                uart_putchar('D'); // tilt right = frontflip
                past_y = 1;
                for (volatile int i = 0; i < 200000; i++);
            } else if (y < -45 && past_y != -1) {
                uart_putchar('A'); // tilt left = backflip
                past_y = -1;
                for (volatile int i = 0; i < 200000; i++);
            } else if ((y < 15 && past_y > 0) || (y > -15 && past_y < 0)) {
                past_y = 0;
            }
        }
    }
}
