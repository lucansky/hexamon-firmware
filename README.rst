helloworld firmware
====================

Experimental version of Hello world firmware

Written in http://ivorylang.org/

Requires
--------

- stack https://docs.haskellstack.org/en/stable/README/
- arm-none-eabi-newlib
- arm-none-eabi-gcc

Fedora::

  dnf install arm-none-eabi-gcc-cs arm-none-eabi-newlib

Building
--------

To build all images::

  ./standalone-setup.sh # required if you don't have checked out ivory tower and ivory-tower-stm32 repos in .. (recommended)
  make

Tests
-----

Blink
  Blinks red and blue LEDs on GPIOD14 and GPIOD15
CANSendRecv
  Test application sending packets from CAN1, blinks on received packets.
CAN2UART
  Test application for receiving and sending
  CAN packets controlled by UART


Run `make` to build all test applications.
Specific application can be built with `make APP`
loaded with `make APP-load` and `make APP-run`.

To load Blink test application run::

        make blink-test-load

to also issue run and start application after loading use::

        make blink-test-run

to just run gdb with new binary without loading::

        make blink-test-gdb
        # issuing 'load' in gdb         == blink-test-load
        # running both 'load' and 'run' == blink-test-run


Flashing
--------

Manually with BlackMagic Probe::

  arm-none-eabi-gdb --ex 'target extended-remote /dev/ttyACM0' --ex 'monitor swdp_scan' --ex 'attach 1' --ex 'load' build/can2uart-test/image
