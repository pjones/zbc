# zigbee-commander -- Manage a Network of ZigBee Devices

This is a very incomplete (yet working) Haskell library and
command-line tool for controlling a network of ZigBee/XBee devices.
In order to work, this tool assumes there is a ZigBee device connected
to a USB port which it can command with `AT` frames.

The basic idea is to start a "server" with a YAML configuration file.
(See the `examples` directory for example configuration files.)

As events in the network fire, the configuration file tells the
commander how to respond.  You can respond by changing the state of a
node or by running a shell command.

## Forked Dependencies

The `vendor` directory contains changes to the `serialport` package
and the `zigbee-znet25` package.  I sent pull requests/patches to the
upstream packages but they both seem to be inactive.

That means you need to build using my forked copies in `vendor`.
