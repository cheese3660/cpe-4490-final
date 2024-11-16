import serial
from dataclasses import dataclass


def compact_value(value: int) -> bytes:
    # Assume sending in big endian for ease of shift registering
    compact = [value & 0xff]
    value >>= 8
    while value > 0:
        compact.append(value & 0xff)
        value >>= 8
    compact.reverse()
    return bytes(compact)

def bytes_to_int(value: bytes) -> int:
    result = 0
    for v in value:
        result <<= 8
        result |= int(v)
    return result

@dataclass
class VpeReceiveReport:
    """Information about received data"""
    receiveTime: int # In nanoseconds
    receiveData: bytes

class VpeSerialAdapter:
    """Adapt a serial port to be used for VPE transmission"""
    def __init__(self, port: str, timeout: float | None = None) -> None:
        """Open a serial adapter on a com port"""
        self.port = serial.Serial(port=port,baudrate=115200, timeout=timeout)
    
    def send(self, data: bytes, t0: int = 100000) -> None:
        """
            Send a block of data over this adapter at a given t0 rate

            t0 is in nanoseconds
        """
        t0_compact = compact_value(t0)
        len_compact = compact_value(len(data))
        buffer = bytes([len(t0_compact)]) + t0_compact + bytes([len(len_compact)]) + len_compact + data
        print(f"sending {len(buffer)} bytes (in hex {hex(len(buffer))})")
        print(f"data size {len(data)} (in hex {hex(len(data))})")
        self.port.write(buffer)
    
    def receive(self) -> VpeReceiveReport:
        """Receive a block of data from this adapter"""
        time_length_bytes = self.port.read(1)
        if len(time_length_bytes) != 1:
            raise TimeoutError()
        time_length = int(time_length_bytes[0])
        time_bytes = self.port.read(time_length)
        if len(time_bytes) != time_length:
            raise TimeoutError()
        time = bytes_to_int(time_bytes)
        
        count_length_bytes = self.port.read(1)
        if len(count_length_bytes) != 1:
            raise TimeoutError()
        count_length = int(count_length_bytes[0])

        count_bytes = self.port.read(count_length)
        if len(count_bytes) != count_length:
            raise TimeoutError()
        count = bytes_to_int(count_length)
        print(f"receiving {count} bytes (transmission took {time} ns)")
        data = self.port.read(count)
        if len(data) != count:
            raise TimeoutError()

        return VpeReceiveReport(time,data)
    
    def close(self) -> None:
        """Close the adapter"""
        self.port.close()