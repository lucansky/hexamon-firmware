HEXAMON.io MCU firmware
====================

Hexamon firmware for STM32F042.
Dual channel HX711 periodic weight measuring, sends acquired data over CAN.

- PA1 -- shared CLK for HX711 modules
- PA4 -- DATA pin HX711 module 0
- PA5 -- DATA pin HX711 module 1

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

Run `make hexamon` to build application.

To load Hexamon firmware run::

        make hexamon-load

to also issue run and start application after loading use::

        make hexamon-run

to just run gdb with new binary without loading::

        make hexamon-gdb
        # issuing 'load' in gdb         == hexamon-load
        # running both 'load' and 'run' == hexamon-run

Flashing
--------

Manually with BlackMagic Probe::

  arm-none-eabi-gdb --ex 'target extended-remote /dev/ttyACM0' --ex 'monitor swdp_scan' --ex 'attach 1' --ex 'load' build/hexamon/image
