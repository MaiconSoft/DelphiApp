# FimataPascal version Delphi

Adaptation of project of [Sottam](https://github.com/sottam/FirmataPascal) for Delphi compatibility.

## Requeriments
ComPort-Libray from [CWBudde](https://github.com/CWBudde/ComPort-Library) adaptation of [dejancrn, dybdahl, wpostma](https://sourceforge.net/projects/comport).

## Compatibility
* RAD Studio 10.2 Tokyo.

## Protocol
The firmata documentation is hosted  [here](https://github.com/firmata/protocol/blob/master/protocol.md "Basic")  for basic and 
[here](https://github.com/firmata/protocol/blob/master/i2c.md "here") for I2C.

## Documentation

### Digitals

#### PWM - AnalogWrite

The property **"Analog[pin]"** can be used for read as analog value, but also used for write as PWM in a pin. If the pin not support PWM then the command will be ignored, othewise the state of pin will change automatically for **pmPWM**.

- Set PWM of pin 13 to 127(50%):

``` Pascal
	var
		Arduino:TFirmata;
	...
	begin
		Arduino.Analog[13] = 127;	
	end;
```
Other way to do this exemple is with **"AnalogRelative[pin]"**, in this property the values are percentages of maximum value. Is usefull for hardwares with PWM and ADC different of default (PWM:8 bits and ADC:10 bits):

- Set PWM of pin 13 to 50% (127 if PWM is 8 bits):

``` Pascal
	var
		Arduino:TFirmata;
	...
	begin
		Arduino.AnalogRelative[13] = 50;	
	end;
```

### Analogs

The component TFormata have a property "analogs" defined by user, for eneable receve notifications of analog values changes. If no one is checked, no "OnAnalogChange" never be called.

The pins Analogs can be acessed by use the hex number 0xA**X**, where **X** is the analog channel in hex (0 to F). This is usefull for acess analog pins in digital functions, like digitaRead, for exemple:

- Set pin A0 as digital and read it:

``` Pascal
	var
		Arduino:TFirmata;
	...
	begin
		Arduino.PinMode[$A0] := pmInput;
		if(Arduino.Digital[$A0] = psHigh)
		begin
			ShowMessage('Analog 0 is active now');
		end;
	end;
```


## Status
<img src="https://d2v9ipibika81v.cloudfront.net/uploads/sites/240/2017/04/warning750.png" width="48"> In developer.

### Implemented:

* Digital;
* Analogic;
* Serial:
	* Software;
	* Hardware.
* I2C;

### Tested:

* Digital;
* Analogic;