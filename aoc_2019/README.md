# FPGA Solution

This year's challenge is to do the entire thing on an FPGA, in this case in
VHDL.

The rules are simple - the day/part can be hard-coded, or set with switches,
but the entire input must be streamed to the FPGA as downloaded.  No
pre-processing to parse integers or translate characters is allowed.

I have a [DE-10 development board][DE-10] development board to run it on, this
uses Intel/Altera's Cyclone V FPGA.

## Building

The simulation tests can be run with `./modelsim/test.sh`.  These test that the
outputs match my input (not included), so you will have to edit the expected
output to pass the tests.

To build the FPGA command stream and program it, run `./quartus/build.sh` and
then `./quartus/program.sh`.

## Running

Once the FPGA is programmed, open a serial connection to set the serial port
parameters, and receive the answer. I recommend `picocom`:

```
picocom -b 115200 --imap lfcrlf /dev/ttyUSB0
```

Then, set the day/part using the physical switches. Finally, send the input:

```
tee /dev/ttyUSB0 < day1.txt
```

The answer should appear on the `picocom` terminal. Between inputs, reset the
state of the FPGA with the reset button.

[DE-10]: https://www.terasic.com.tw/cgi-bin/page/archive.pl?Language=English&CategoryNo=167&No=1081                                                               
