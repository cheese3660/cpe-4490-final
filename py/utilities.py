import serial
import glob
import sys
import argparse
import serial.tools
import serial.tools.list_ports
import vpe


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
    d = f.read()
    f.close()
    port = vpe.VpeSerialAdapter(args.port)
    port.send(d, int(args.timebase))
    port.close()

def receive_file_subprogram():
    if not args.file:
        print("Expected a file name to receive!")
    

    port = vpe.VpeSerialAdapter(args.port)
    report = port.receive()
    data = report.receiveData
    print(f"received {len(data)} in bytes {report.receiveTime} ns ({int(len(data) / (report.receiveTime / 1_000_000_000.0))} B/s)")

    f = open(args.file, "wb")
    f.write(data)
    f.close()

def round_trip_subprogram():
    if not args.file:
        print("Expected a file name to round trip!")
    f = open(args.file,"rb")
    d = f.read()
    f.close()
    port = vpe.VpeSerialAdapter(args.port)
    port.send(d, int(args.timebase))
    report = port.receive()
    data = report.receiveData
    print(f"round tripped {len(data)} bytes in {report.receiveTime} ns ({int(len(data) / (report.receiveTime / 1_000_000_000.0))} B/s)")

    if d == data:
        print("round trip successful!")
    else:
        print("round trip failed!")
    port.close()

def repl_subprogram():
    port = vpe.VpeSerialAdapter(args.port,30)
    currentT0 = int(args.timebase)
    print("/---------------------------------------------\\")
    print("| VPE SHELL                                   |")
    print("|                                             |")
    print("| T0=... (without spaces) to set the T0 value |")
    print("| EXIT to quit                                |")
    print("| anything else to round trip that data       |")
    print("\\---------------------------------------------/")
    while True:
        print("> ",end="")
        cmd = input()
        if cmd.lower() == "exit":
            break
        elif cmd.startswith("T0="):
            currentT0 = int(cmd[3:])
        else:
            text = cmd.encode('utf-8')
            print(f"Sending: {cmd}")
            port.send(text, currentT0)
            print(f"Waiting to receive...")
            try:
                report = port.receive()
                data = report.receiveData
                print(f"Got {len(data)} bytes in {report.receiveTime} ns ({int(len(data) / (report.receiveTime / 1_000_000_000.0))} B/s)")
                result = data.decode('utf-8')
                print(f"Received: {result}")
            except TimeoutError:
                print("Timed out...")
    port.close()

if args.subprogram == "list":
    list_ports_subprogram()
elif args.subprogram == "header":
    send_header_subprogram()
elif args.subprogram == "send":
    send_file_subprogram()
elif args.subprogram == "repl":
    repl_subprogram()
elif args.subprogram == "receive":
    receive_file_subprogram()
elif args.subprogram == "round-trip":
    round_trip_subprogram()
else:
    print("Invalid subprogram")