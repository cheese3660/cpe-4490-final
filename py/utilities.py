import serial
import glob
import sys
import argparse
import serial.tools
import serial.tools.list_ports


parser = argparse.ArgumentParser(
    prog = "VPE Python Utilities",
    description = "A small list of utilities for testing the serial interface for the basys3"
)
parser.add_argument("subprogram")
parser.add_argument("-p", "--port", default="")
parser.add_argument("-c", "--count",default=0)
parser.add_argument("-t", "--timebase", default=100_000)
parser.add_argument("-f", "--file",default="")
args = parser.parse_args()


def list_ports():
    ports = serial.tools.list_ports.comports()
    return ports


def get_first_port():
    return sorted(list_ports())[0].name

args.port = args.port if args.port else get_first_port()


def list_ports_subprogram():
    for port, desc, hwid in sorted(list_ports()):
        print("{}: {} [{}]".format(port, desc, hwid))

def compact_value(value: int):
    # Assume sending in big endian for ease of shift registering
    compact = [value & 0xff]
    value >>= 8
    while value > 0:
        compact.append(value & 0xff)
        value >>= 8
    compact.reverse()
    return bytearray(compact)


def send_header_subprogram():
    #print(compact_value(int(args.timebase)))
    #print(compact_value(int(args.count)))
    # Lets build the buffer
    buffer = bytearray()
    tb = compact_value(int(args.timebase))
    buffer.append(len(tb))
    buffer += tb
    cnt = compact_value(int(args.count))
    buffer.append(len(cnt))
    buffer += cnt
    print(f"sending {len(buffer)} bytes (in hex {hex(len(buffer))})")
    print(f"file size {int(args.count)} (in hex {hex(int(args.count))})")
    print(", ".join([hex(int(x)) for x in buffer]))
    s = serial.Serial(args.port, baudrate=115200)
    s.write(buffer)
    s.close()

def send_file_subprogram():
    if not args.file:
        print("Expected a file name to send!")
    f = open(args.file,"rb")
    d = bytearray(f.read())
    f.close()
    buffer = bytearray()
    tb = compact_value(int(args.timebase))
    buffer.append(len(tb))
    buffer += tb
    cnt = compact_value(len(d))
    buffer.append(len(cnt))
    buffer += cnt
    buffer += d
    print(f"sending {len(buffer)} bytes (in hex {hex(len(buffer))})")
    print(f"file size {len(d)} (in hex {hex(len(d))})")
    s = serial.Serial(args.port, baudrate=115200)
    s.write(buffer)
    s.close()

if args.subprogram == "list":
    list_ports_subprogram()
elif args.subprogram == "header":
    send_header_subprogram()
elif args.subprogram == "file":
    send_file_subprogram()
else:
    print("Invalid subprogram")