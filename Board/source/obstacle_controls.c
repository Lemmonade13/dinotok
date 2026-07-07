#include "MKL46Z4.h"
#include <stdint.h>
#include "init.h"

void green_on_frdm(void){
	PTD->PCOR = 1<<5;
}
void green_off_frdm(void){
	PTD->PSOR = 1<<5;
}

int main(void) {
    buttons_init();
    led_init();
    uart_init();

    while (1) {
        
        // check if OCaml sent a message to control Player 2 LED
        // 1: obstacle can be placed => LED turns on
        // 0: obstacle can't be placed => LED turns off
    	// we couldn't actually get this code to fully work
    	// so rn the led stuff does nothing
        char message = uart_getchar_nonblocking();
        if (message == '1') {
            //green_on_frdm();
        	//GPIOD->PSOR = (1<<5);      // LED on
        } else if (message == '0') {
        	//green_off_frdm();
            //GPIOD->PCOR = (1<<5);      // LED off
        }

        // Upper obstacle button (when pressed, it sends 'H' to OCaml)
        if (!(GPIOC->PDIR & (1<<3))) {
            uart_putchar('H');
            for (volatile int i = 0; i < 500000; i++);
        }

        // Lower obstacle button (when pressed, it sends 'L' to OCaml)
        if (!(GPIOC->PDIR & (1<<12))) {
            uart_putchar('L');
            for (volatile int i = 0; i < 500000; i++);
        }

    }
}
