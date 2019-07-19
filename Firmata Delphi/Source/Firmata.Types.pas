unit Firmata.Types;

interface

type
  TPin = record
    mode: byte;
    analog_channel: byte;
    supported_modes: UInt64;
    value: integer;
  end;

type
  TCallback = procedure(message: array of byte);

implementation

end.
