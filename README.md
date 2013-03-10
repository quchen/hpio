GPIO
====

GPIO is a library to interface /sys/class/gpio on Linux. It aims to be a two-level library, with a lower-level end that is only a thin and unsafe wrapper around writing to the gpio files, and a high-level one that handles exceptions and allocations of the pins.
