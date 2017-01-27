;;;; frc-hal.lisp

(in-package #:frc-hal)

(define-foreign-library libhalathena
  (t (:default "libHALAthena")))
(use-foreign-library libhalathena)

;;; Semaphore.hpp

(cffi:defcfun ("initializeMutexNormal" initializeMutexNormal) :pointer)

(cffi:defcfun ("deleteMutex" deleteMutex) :void
  (sem :pointer))

(cffi:defcfun ("takeMutex" takeMutex) :void
  (sem :pointer))

(cffi:defcfun ("tryTakeMutex" tryTakeMutex) :pointer
  (sem :pointer))

(cffi:defcfun ("giveMutex" giveMutex) :void
  (sem :pointer))

(cffi:defcfun ("initializeMultiWait" initializeMultiWait) :pointer)

(cffi:defcfun ("deleteMultiWait" deleteMultiWait) :void
  (sem :pointer))

(cffi:defcfun ("takeMultiWait" takeMultiWait) :void
  (sem :pointer)
  (m :pointer))

(cffi:defcfun ("giveMultiWait" giveMultiWait) :void
  (sem :pointer))

;;; HAL

(cffi:defcfun ("getPort" getPort) :pointer
  (pin :pointer))

(cffi:defcfun ("getPortWithModule" getPortWithModule) :pointer
  (module :pointer)
  (pin :pointer))

(cffi:defcfun ("freePort" freePort) :void
  (port :pointer))

(cffi:defcfun ("getHALErrorMessage" getHALErrorMessage) :string
  (code :pointer))

(cffi:defcfun ("getFPGAVersion" getFPGAVersion) :pointer
  (status :pointer))

(cffi:defcfun ("getFPGARevision" getFPGARevision) :pointer
  (status :pointer))

(cffi:defcfun ("getFPGATime" getFPGATime) :pointer
  (status :pointer))

(cffi:defcfun ("getFPGAButton" getFPGAButton) :pointer
  (status :pointer))

(cffi:defcfun ("HALSetErrorData" HALSetErrorData) :int
  (errors :string)
  (errorsLength :int)
  (wait_ms :int))

(cffi:defcfun ("HALSendError" HALSendError) :int
  (isError :int)
  (errorCode :pointer)
  (isLVCode :int)
  (details :string)
  (location :string)
  (callStack :string)
  (printMsg :int))

(cffi:defcfun ("HALGetControlWord" HALGetControlWord) :int
  (data :pointer))

(cffi:defcfun ("HALGetAllianceStation" HALGetAllianceStation) :int
  (allianceStation :pointer))

(cffi:defcfun ("HALGetJoystickAxes" HALGetJoystickAxes) :int
  (joystickNum :pointer)
  (axes :pointer))

(cffi:defcfun ("HALGetJoystickPOVs" HALGetJoystickPOVs) :int
  (joystickNum :pointer)
  (povs :pointer))

(cffi:defcfun ("HALGetJoystickButtons" HALGetJoystickButtons) :int
  (joystickNum :pointer)
  (buttons :pointer))

(cffi:defcfun ("HALGetJoystickDescriptor" HALGetJoystickDescriptor) :int
  (joystickNum :pointer)
  (desc :pointer))

(cffi:defcfun ("HALGetJoystickIsXbox" HALGetJoystickIsXbox) :int
  (joystickNum :pointer))

(cffi:defcfun ("HALGetJoystickType" HALGetJoystickType) :int
  (joystickNum :pointer))

(cffi:defcfun ("HALGetJoystickName" HALGetJoystickName) :string
  (joystickNum :pointer))

(cffi:defcfun ("HALGetJoystickAxisType" HALGetJoystickAxisType) :int
  (joystickNum :pointer)
  (axis :pointer))

(cffi:defcfun ("HALSetJoystickOutputs" HALSetJoystickOutputs) :int
  (joystickNum :pointer)
  (outputs :pointer)
  (leftRumble :pointer)
  (rightRumble :pointer))

(cffi:defcfun ("HALGetMatchTime" HALGetMatchTime) :int
  (matchTime :pointer))

(cffi:defcfun ("HALSetNewDataSem" HALSetNewDataSem) :void
  (sem :pointer))

(cffi:defcfun ("HALGetSystemActive" HALGetSystemActive) :pointer
  (status :pointer))

(cffi:defcfun ("HALGetBrownedOut" HALGetBrownedOut) :pointer
  (status :pointer))

(cffi:defcfun ("HALInitialize" HALInitialize) :int
  (mode :int))

(cffi:defcfun ("HALNetworkCommunicationObserveUserProgramStarting" HALNetworkCommunicationObserveUserProgramStarting) :void)

(cffi:defcfun ("HALNetworkCommunicationObserveUserProgramDisabled" HALNetworkCommunicationObserveUserProgramDisabled) :void)

(cffi:defcfun ("HALNetworkCommunicationObserveUserProgramAutonomous" HALNetworkCommunicationObserveUserProgramAutonomous) :void)

(cffi:defcfun ("HALNetworkCommunicationObserveUserProgramTeleop" HALNetworkCommunicationObserveUserProgramTeleop) :void)

(cffi:defcfun ("HALNetworkCommunicationObserveUserProgramTest" HALNetworkCommunicationObserveUserProgramTest) :void)

(cffi:defcfun ("HALReport" HALReport) :pointer
  (resource :pointer)
  (instanceNumber :pointer)
  (context :pointer)
  (feature :string))

;;; Accelerometer

(cffi:defcfun ("setAccelerometerActive" setAccelerometerActive) :void
  (arg0 :pointer))

(cffi:defcfun ("setAccelerometerRange" setAccelerometerRange) :void
  (arg0 :pointer 
   ;;AccelerometerRange
   ))

(cffi:defcfun ("getAccelerometerX" getAccelerometerX) :double)

(cffi:defcfun ("getAccelerometerY" getAccelerometerY) :double)

(cffi:defcfun ("getAccelerometerZ" getAccelerometerZ) :double)

;;;; Analog
;;; Output

(cffi:defcfun ("initializeAnalogOutputPort" initializeAnalogOutputPort) :pointer
  (port_pointer :pointer)
  (status :pointer))

(cffi:defcfun ("freeAnalogOutputPort" freeAnalogOutputPort) :void
  (analog_port_pointer :pointer))

(cffi:defcfun ("setAnalogOutput" setAnalogOutput) :void
  (analog_port_pointer :pointer)
  (voltage :double)
  (status :pointer))

(cffi:defcfun ("getAnalogOutput" getAnalogOutput) :double
  (analog_port_pointer :pointer)
  (status :pointer))

(cffi:defcfun ("checkAnalogOutputChannel" checkAnalogOutputChannel) :pointer
  (pin :pointer))

;;;Input
(cffi:defcfun ("initializeAnalogInputPort" initializeAnalogInputPort) :pointer
  (port_pointer :pointer)
  (status :pointer))

(cffi:defcfun ("freeAnalogInputPort" freeAnalogInputPort) :void
  (analog_port_pointer :pointer))

(cffi:defcfun ("checkAnalogModule" checkAnalogModule) :pointer
  (module :pointer))

(cffi:defcfun ("checkAnalogInputChannel" checkAnalogInputChannel) :pointer
  (pin :pointer))

(cffi:defcfun ("setAnalogSampleRate" setAnalogSampleRate) :void
  (samplesPerSecond :double)
  (status :pointer))

(cffi:defcfun ("getAnalogSampleRate" getAnalogSampleRate) :float
  (status :pointer))

(cffi:defcfun ("setAnalogAverageBits" setAnalogAverageBits) :void
  (analog_port_pointer :pointer)
  (bits :pointer)
  (status :pointer))

(cffi:defcfun ("getAnalogAverageBits" getAnalogAverageBits) :pointer
  (analog_port_pointer :pointer)
  (status :pointer))

(cffi:defcfun ("setAnalogOversampleBits" setAnalogOversampleBits) :void
  (analog_port_pointer :pointer)
  (bits :pointer)
  (status :pointer))

(cffi:defcfun ("getAnalogOversampleBits" getAnalogOversampleBits) :pointer
  (analog_port_pointer :pointer)
  (status :pointer))

(cffi:defcfun ("getAnalogValue" getAnalogValue) :pointer
  (analog_port_pointer :pointer)
  (status :pointer))

(cffi:defcfun ("getAnalogAverageValue" getAnalogAverageValue) :pointer
  (analog_port_pointer :pointer)
  (status :pointer))

(cffi:defcfun ("getAnalogVoltsToValue" getAnalogVoltsToValue) :pointer
  (analog_port_pointer :pointer)
  (voltage :double)
  (status :pointer))

(cffi:defcfun ("getAnalogVoltage" getAnalogVoltage) :float
  (analog_port_pointer :pointer)
  (status :pointer))

(cffi:defcfun ("getAnalogAverageVoltage" getAnalogAverageVoltage) :float
  (analog_port_pointer :pointer)
  (status :pointer))

(cffi:defcfun ("getAnalogLSBWeight" getAnalogLSBWeight) :pointer
  (analog_port_pointer :pointer)
  (status :pointer))

(cffi:defcfun ("getAnalogOffset" getAnalogOffset) :pointer
  (analog_port_pointer :pointer)
  (status :pointer))

(cffi:defcfun ("isAccumulatorChannel" isAccumulatorChannel) :pointer
  (analog_port_pointer :pointer)
  (status :pointer))

(cffi:defcfun ("initAccumulator" initAccumulator) :void
  (analog_port_pointer :pointer)
  (status :pointer))

(cffi:defcfun ("resetAccumulator" resetAccumulator) :void
  (analog_port_pointer :pointer)
  (status :pointer))

(cffi:defcfun ("setAccumulatorCenter" setAccumulatorCenter) :void
  (analog_port_pointer :pointer)
  (center :pointer)
  (status :pointer))

(cffi:defcfun ("setAccumulatorDeadband" setAccumulatorDeadband) :void
  (analog_port_pointer :pointer)
  (deadband :pointer)
  (status :pointer))

(cffi:defcfun ("getAccumulatorValue" getAccumulatorValue) :pointer
  (analog_port_pointer :pointer)
  (status :pointer))

(cffi:defcfun ("getAccumulatorCount" getAccumulatorCount) :pointer
  (analog_port_pointer :pointer)
  (status :pointer))

(cffi:defcfun ("getAccumulatorOutput" getAccumulatorOutput) :void
  (analog_port_pointer :pointer)
  (value :pointer)
  (count :pointer)
  (status :pointer))

(cffi:defcfun ("initializeAnalogTrigger" initializeAnalogTrigger) :pointer
  (port_pointer :pointer)
  (index :pointer)
  (status :pointer))

(cffi:defcfun ("cleanAnalogTrigger" cleanAnalogTrigger) :void
  (analog_trigger_pointer :pointer)
  (status :pointer))

(cffi:defcfun ("setAnalogTriggerLimitsRaw" setAnalogTriggerLimitsRaw) :void
  (analog_trigger_pointer :pointer)
  (lower :pointer)
  (upper :pointer)
  (status :pointer))

(cffi:defcfun ("setAnalogTriggerLimitsVoltage" setAnalogTriggerLimitsVoltage) :void
  (analog_trigger_pointer :pointer)
  (lower :double)
  (upper :double)
  (status :pointer))

(cffi:defcfun ("setAnalogTriggerAveraged" setAnalogTriggerAveraged) :void
  (analog_trigger_pointer :pointer)
  (useAveragedValue :pointer)
  (status :pointer))

(cffi:defcfun ("setAnalogTriggerFiltered" setAnalogTriggerFiltered) :void
  (analog_trigger_pointer :pointer)
  (useFilteredValue :pointer)
  (status :pointer))

(cffi:defcfun ("getAnalogTriggerInWindow" getAnalogTriggerInWindow) :pointer
  (analog_trigger_pointer :pointer)
  (status :pointer))

(cffi:defcfun ("getAnalogTriggerTriggerState" getAnalogTriggerTriggerState) :pointer
  (analog_trigger_pointer :pointer)
  (status :pointer))

(cffi:defcfun ("getAnalogTriggerOutput" getAnalogTriggerOutput) :pointer
  (analog_trigger_pointer :pointer)
  (type :pointer 
   ;;AnalogTriggerType
   )
  (status :pointer))

;;; Compressor

(cffi:defcfun ("initializeCompressor" initializeCompressor) :pointer
  (module :pointer))

(cffi:defcfun ("checkCompressorModule" checkCompressorModule) :pointer
  (module :pointer))

(cffi:defcfun ("getCompressor" getCompressor) :pointer
  (pcm_pointer :pointer)
  (status :pointer))

(cffi:defcfun ("setClosedLoopControl" setClosedLoopControl) :void
  (pcm_pointer :pointer)
  (value :pointer)
  (status :pointer))

(cffi:defcfun ("getClosedLoopControl" getClosedLoopControl) :pointer
  (pcm_pointer :pointer)
  (status :pointer))

(cffi:defcfun ("getPressureSwitch" getPressureSwitch) :pointer
  (pcm_pointer :pointer)
  (status :pointer))

(cffi:defcfun ("getCompressorCurrent" getCompressorCurrent) :float
  (pcm_pointer :pointer)
  (status :pointer))

(cffi:defcfun ("getCompressorCurrentTooHighFault" getCompressorCurrentTooHighFault) :pointer
  (pcm_pointer :pointer)
  (status :pointer))

(cffi:defcfun ("getCompressorCurrentTooHighStickyFault" getCompressorCurrentTooHighStickyFault) :pointer
  (pcm_pointer :pointer)
  (status :pointer))

(cffi:defcfun ("getCompressorShortedStickyFault" getCompressorShortedStickyFault) :pointer
  (pcm_pointer :pointer)
  (status :pointer))

(cffi:defcfun ("getCompressorShortedFault" getCompressorShortedFault) :pointer
  (pcm_pointer :pointer)
  (status :pointer))

(cffi:defcfun ("getCompressorNotConnectedStickyFault" getCompressorNotConnectedStickyFault) :pointer
  (pcm_pointer :pointer)
  (status :pointer))

(cffi:defcfun ("getCompressorNotConnectedFault" getCompressorNotConnectedFault) :pointer
  (pcm_pointer :pointer)
  (status :pointer))

(cffi:defcfun ("clearAllPCMStickyFaults" clearAllPCMStickyFaults) :void
  (pcm_pointer :pointer)
  (status :pointer))

;;; Digital

(cffi:defcfun ("initializeDigitalPort" initializeDigitalPort) :pointer
  (port_pointer :pointer)
  (status :pointer))

(cffi:defcfun ("freeDigitalPort" freeDigitalPort) :void
  (digital_port_pointer :pointer))

(cffi:defcfun ("checkPWMChannel" checkPWMChannel) :pointer
  (digital_port_pointer :pointer))

(cffi:defcfun ("checkRelayChannel" checkRelayChannel) :pointer
  (digital_port_pointer :pointer))

(cffi:defcfun ("setPWM" setPWM) :void
  (digital_port_pointer :pointer)
  (value :unsigned-short)
  (status :pointer))

(cffi:defcfun ("allocatePWMChannel" allocatePWMChannel) :pointer
  (digital_port_pointer :pointer)
  (status :pointer))

(cffi:defcfun ("freePWMChannel" freePWMChannel) :void
  (digital_port_pointer :pointer)
  (status :pointer))

(cffi:defcfun ("getPWM" getPWM) :unsigned-short
  (digital_port_pointer :pointer)
  (status :pointer))

(cffi:defcfun ("latchPWMZero" latchPWMZero) :void
  (digital_port_pointer :pointer)
  (status :pointer))

(cffi:defcfun ("setPWMPeriodScale" setPWMPeriodScale) :void
  (digital_port_pointer :pointer)
  (squelchMask :pointer)
  (status :pointer))

(cffi:defcfun ("allocatePWM" allocatePWM) :pointer
  (status :pointer))

(cffi:defcfun ("freePWM" freePWM) :void
  (pwmGenerator :pointer)
  (status :pointer))

(cffi:defcfun ("setPWMRate" setPWMRate) :void
  (rate :double)
  (status :pointer))

(cffi:defcfun ("setPWMDutyCycle" setPWMDutyCycle) :void
  (pwmGenerator :pointer)
  (dutyCycle :double)
  (status :pointer))

(cffi:defcfun ("setPWMOutputChannel" setPWMOutputChannel) :void
  (pwmGenerator :pointer)
  (pin :pointer)
  (status :pointer))

(cffi:defcfun ("setRelayForward" setRelayForward) :void
  (digital_port_pointer :pointer)
  (on :pointer)
  (status :pointer))

(cffi:defcfun ("setRelayReverse" setRelayReverse) :void
  (digital_port_pointer :pointer)
  (on :pointer)
  (status :pointer))

(cffi:defcfun ("getRelayForward" getRelayForward) :pointer
  (digital_port_pointer :pointer)
  (status :pointer))

(cffi:defcfun ("getRelayReverse" getRelayReverse) :pointer
  (digital_port_pointer :pointer)
  (status :pointer))

(cffi:defcfun ("allocateDIO" allocateDIO) :pointer
  (digital_port_pointer :pointer)
  (input :pointer)
  (status :pointer))

(cffi:defcfun ("freeDIO" freeDIO) :void
  (digital_port_pointer :pointer)
  (status :pointer))

(cffi:defcfun ("setDIO" setDIO) :void
  (digital_port_pointer :pointer)
  (value :short)
  (status :pointer))

(cffi:defcfun ("getDIO" getDIO) :pointer
  (digital_port_pointer :pointer)
  (status :pointer))

(cffi:defcfun ("getDIODirection" getDIODirection) :pointer
  (digital_port_pointer :pointer)
  (status :pointer))

(cffi:defcfun ("pulse" pulse) :void
  (digital_port_pointer :pointer)
  (pulseLength :double)
  (status :pointer))

(cffi:defcfun ("isPulsing" isPulsing) :pointer
  (digital_port_pointer :pointer)
  (status :pointer))

(cffi:defcfun ("isAnyPulsing" isAnyPulsing) :pointer
  (status :pointer))

(cffi:defcfun ("setFilterSelect" setFilterSelect) :void
  (digital_port_pointer :pointer)
  (filter_index :int)
  (status :pointer))

(cffi:defcfun ("getFilterSelect" getFilterSelect) :int
  (digital_port_pointer :pointer)
  (status :pointer))

(cffi:defcfun ("setFilterPeriod" setFilterPeriod) :void
  (filter_index :int)
  (value :pointer)
  (status :pointer))

(cffi:defcfun ("getFilterPeriod" getFilterPeriod) :pointer
  (filter_index :int)
  (status :pointer))

(cffi:defcfun ("initializeCounter" initializeCounter) :pointer
  (mode :pointer 
   ;;Mode
   )
  (index :pointer)
  (status :pointer))

(cffi:defcfun ("freeCounter" freeCounter) :void
  (counter_pointer :pointer)
  (status :pointer))

(cffi:defcfun ("setCounterAverageSize" setCounterAverageSize) :void
  (counter_pointer :pointer)
  (size :pointer)
  (status :pointer))

(cffi:defcfun ("setCounterUpSource" setCounterUpSource) :void
  (counter_pointer :pointer)
  (pin :pointer)
  (analogTrigger :pointer)
  (status :pointer))

(cffi:defcfun ("setCounterUpSourceEdge" setCounterUpSourceEdge) :void
  (counter_pointer :pointer)
  (risingEdge :pointer)
  (fallingEdge :pointer)
  (status :pointer))

(cffi:defcfun ("clearCounterUpSource" clearCounterUpSource) :void
  (counter_pointer :pointer)
  (status :pointer))

(cffi:defcfun ("setCounterDownSource" setCounterDownSource) :void
  (counter_pointer :pointer)
  (pin :pointer)
  (analogTrigger :pointer)
  (status :pointer))

(cffi:defcfun ("setCounterDownSourceEdge" setCounterDownSourceEdge) :void
  (counter_pointer :pointer)
  (risingEdge :pointer)
  (fallingEdge :pointer)
  (status :pointer))

(cffi:defcfun ("clearCounterDownSource" clearCounterDownSource) :void
  (counter_pointer :pointer)
  (status :pointer))

(cffi:defcfun ("setCounterUpDownMode" setCounterUpDownMode) :void
  (counter_pointer :pointer)
  (status :pointer))

(cffi:defcfun ("setCounterExternalDirectionMode" setCounterExternalDirectionMode) :void
  (counter_pointer :pointer)
  (status :pointer))

(cffi:defcfun ("setCounterSemiPeriodMode" setCounterSemiPeriodMode) :void
  (counter_pointer :pointer)
  (highSemiPeriod :pointer)
  (status :pointer))

(cffi:defcfun ("setCounterPulseLengthMode" setCounterPulseLengthMode) :void
  (counter_pointer :pointer)
  (threshold :double)
  (status :pointer))

(cffi:defcfun ("getCounterSamplesToAverage" getCounterSamplesToAverage) :pointer
  (counter_pointer :pointer)
  (status :pointer))

(cffi:defcfun ("setCounterSamplesToAverage" setCounterSamplesToAverage) :void
  (counter_pointer :pointer)
  (samplesToAverage :int)
  (status :pointer))

(cffi:defcfun ("resetCounter" resetCounter) :void
  (counter_pointer :pointer)
  (status :pointer))

(cffi:defcfun ("getCounter" getCounter) :pointer
  (counter_pointer :pointer)
  (status :pointer))

(cffi:defcfun ("getCounterPeriod" getCounterPeriod) :double
  (counter_pointer :pointer)
  (status :pointer))

(cffi:defcfun ("setCounterMaxPeriod" setCounterMaxPeriod) :void
  (counter_pointer :pointer)
  (maxPeriod :double)
  (status :pointer))

(cffi:defcfun ("setCounterUpdateWhenEmpty" setCounterUpdateWhenEmpty) :void
  (counter_pointer :pointer)
  (enabled :pointer)
  (status :pointer))

(cffi:defcfun ("getCounterStopped" getCounterStopped) :pointer
  (counter_pointer :pointer)
  (status :pointer))

(cffi:defcfun ("getCounterDirection" getCounterDirection) :pointer
  (counter_pointer :pointer)
  (status :pointer))

(cffi:defcfun ("setCounterReverseDirection" setCounterReverseDirection) :void
  (counter_pointer :pointer)
  (reverseDirection :pointer)
  (status :pointer))

(cffi:defcfun ("initializeEncoder" initializeEncoder) :pointer
  (port_a_module :pointer)
  (port_a_pin :pointer)
  (port_a_analog_trigger :pointer)
  (port_b_module :pointer)
  (port_b_pin :pointer)
  (port_b_analog_trigger :pointer)
  (reverseDirection :pointer)
  (index :pointer)
  (status :pointer))

(cffi:defcfun ("freeEncoder" freeEncoder) :void
  (encoder_pointer :pointer)
  (status :pointer))

(cffi:defcfun ("resetEncoder" resetEncoder) :void
  (encoder_pointer :pointer)
  (status :pointer))

(cffi:defcfun ("getEncoder" getEncoder) :pointer
  (encoder_pointer :pointer)
  (status :pointer))

(cffi:defcfun ("getEncoderPeriod" getEncoderPeriod) :double
  (encoder_pointer :pointer)
  (status :pointer))

(cffi:defcfun ("setEncoderMaxPeriod" setEncoderMaxPeriod) :void
  (encoder_pointer :pointer)
  (maxPeriod :double)
  (status :pointer))

(cffi:defcfun ("getEncoderStopped" getEncoderStopped) :pointer
  (encoder_pointer :pointer)
  (status :pointer))

(cffi:defcfun ("getEncoderDirection" getEncoderDirection) :pointer
  (encoder_pointer :pointer)
  (status :pointer))

(cffi:defcfun ("setEncoderReverseDirection" setEncoderReverseDirection) :void
  (encoder_pointer :pointer)
  (reverseDirection :pointer)
  (status :pointer))

(cffi:defcfun ("setEncoderSamplesToAverage" setEncoderSamplesToAverage) :void
  (encoder_pointer :pointer)
  (samplesToAverage :pointer)
  (status :pointer))

(cffi:defcfun ("getEncoderSamplesToAverage" getEncoderSamplesToAverage) :pointer
  (encoder_pointer :pointer)
  (status :pointer))

(cffi:defcfun ("setEncoderIndexSource" setEncoderIndexSource) :void
  (encoder_pointer :pointer)
  (pin :pointer)
  (analogTrigger :pointer)
  (activeHigh :pointer)
  (edgeSensitive :pointer)
  (status :pointer))

(cffi:defcfun ("getLoopTiming" getLoopTiming) :pointer
  (status :pointer))

(cffi:defcfun ("spiInitialize" spiInitialize) :void
  (port :pointer)
  (status :pointer))

(cffi:defcfun ("spiTransaction" spiTransaction) :pointer
  (port :pointer)
  (dataToSend :pointer)
  (dataReceived :pointer)
  (size :pointer))

(cffi:defcfun ("spiWrite" spiWrite) :pointer
  (port :pointer)
  (dataToSend :pointer)
  (sendSize :pointer))

(cffi:defcfun ("spiRead" spiRead) :pointer
  (port :pointer)
  (buffer :pointer)
  (count :pointer))

(cffi:defcfun ("spiClose" spiClose) :void
  (port :pointer))

(cffi:defcfun ("spiSetSpeed" spiSetSpeed) :void
  (port :pointer)
  (speed :pointer))

(cffi:defcfun ("spiSetOpts" spiSetOpts) :void
  (port :pointer)
  (msb_first :int)
  (sample_on_trailing :int)
  (clk_idle_high :int))

(cffi:defcfun ("spiSetChipSelectActiveHigh" spiSetChipSelectActiveHigh) :void
  (port :pointer)
  (status :pointer))

(cffi:defcfun ("spiSetChipSelectActiveLow" spiSetChipSelectActiveLow) :void
  (port :pointer)
  (status :pointer))

(cffi:defcfun ("spiGetHandle" spiGetHandle) :pointer
  (port :pointer))

(cffi:defcfun ("spiSetHandle" spiSetHandle) :void
  (port :pointer)
  (handle :pointer))

(cffi:defcfun ("spiInitAccumulator" spiInitAccumulator) :void
  (port :pointer)
  (period :pointer)
  (cmd :pointer)
  (xfer_size :pointer)
  (valid_mask :pointer)
  (valid_value :pointer)
  (data_shift :pointer)
  (data_size :pointer)
  (is_signed :pointer)
  (big_endian :pointer)
  (status :pointer))

(cffi:defcfun ("spiFreeAccumulator" spiFreeAccumulator) :void
  (port :pointer)
  (status :pointer))

(cffi:defcfun ("spiResetAccumulator" spiResetAccumulator) :void
  (port :pointer)
  (status :pointer))

(cffi:defcfun ("spiSetAccumulatorCenter" spiSetAccumulatorCenter) :void
  (port :pointer)
  (center :pointer)
  (status :pointer))

(cffi:defcfun ("spiSetAccumulatorDeadband" spiSetAccumulatorDeadband) :void
  (port :pointer)
  (deadband :pointer)
  (status :pointer))

(cffi:defcfun ("spiGetAccumulatorLastValue" spiGetAccumulatorLastValue) :pointer
  (port :pointer)
  (status :pointer))

(cffi:defcfun ("spiGetAccumulatorValue" spiGetAccumulatorValue) :pointer
  (port :pointer)
  (status :pointer))

(cffi:defcfun ("spiGetAccumulatorCount" spiGetAccumulatorCount) :pointer
  (port :pointer)
  (status :pointer))

(cffi:defcfun ("spiGetAccumulatorAverage" spiGetAccumulatorAverage) :double
  (port :pointer)
  (status :pointer))

(cffi:defcfun ("spiGetAccumulatorOutput" spiGetAccumulatorOutput) :void
  (port :pointer)
  (value :pointer)
  (count :pointer)
  (status :pointer))

(cffi:defcfun ("i2CInitialize" i2CInitialize) :void
  (port :pointer)
  (status :pointer))

(cffi:defcfun ("i2CTransaction" i2CTransaction) :pointer
  (port :pointer)
  (deviceAddress :pointer)
  (dataToSend :pointer)
  (sendSize :pointer)
  (dataReceived :pointer)
  (receiveSize :pointer))

(cffi:defcfun ("i2CWrite" i2CWrite) :pointer
  (port :pointer)
  (deviceAddress :pointer)
  (dataToSend :pointer)
  (sendSize :pointer))

(cffi:defcfun ("i2CRead" i2CRead) :pointer
  (port :pointer)
  (deviceAddress :pointer)
  (buffer :pointer)
  (count :pointer))

(cffi:defcfun ("i2CClose" i2CClose) :void
  (port :pointer))

;;; Interrupts
(cffi:defcfun ("initializeInterrupts" initializeInterrupts) :pointer
  (interruptIndex :pointer)
  (watcher :pointer)
  (status :pointer))

(cffi:defcfun ("cleanInterrupts" cleanInterrupts) :void
  (interrupt_pointer :pointer)
  (status :pointer))

(cffi:defcfun ("waitForInterrupt" waitForInterrupt) :pointer
  (interrupt_pointer :pointer)
  (timeout :double)
  (ignorePrevious :pointer)
  (status :pointer))

(cffi:defcfun ("enableInterrupts" enableInterrupts) :void
  (interrupt_pointer :pointer)
  (status :pointer))

(cffi:defcfun ("disableInterrupts" disableInterrupts) :void
  (interrupt_pointer :pointer)
  (status :pointer))

(cffi:defcfun ("readRisingTimestamp" readRisingTimestamp) :double
  (interrupt_pointer :pointer)
  (status :pointer))

(cffi:defcfun ("readFallingTimestamp" readFallingTimestamp) :double
  (interrupt_pointer :pointer)
  (status :pointer))

(cffi:defcfun ("requestInterrupts" requestInterrupts) :void
  (interrupt_pointer :pointer)
  (routing_module :pointer)
  (routing_pin :pointer)
  (routing_analog_trigger :pointer)
  (status :pointer))

(cffi:defcfun ("attachInterruptHandler" attachInterruptHandler) :void
  (interrupt_pointer :pointer)
  (handler :pointer)
  (param :pointer)
  (status :pointer))

(cffi:defcfun ("setInterruptUpSourceEdge" setInterruptUpSourceEdge) :void
  (interrupt_pointer :pointer)
  (risingEdge :pointer)
  (fallingEdge :pointer)
  (status :pointer))

;;; Notifier

(cffi:defcfun ("initializeNotifier" initializeNotifier) :pointer
  (process :pointer)
  (param :pointer)
  (status :pointer))

(cffi:defcfun ("cleanNotifier" cleanNotifier) :void
  (notifier_pointer :pointer)
  (status :pointer))

(cffi:defcfun ("getNotifierParam" getNotifierParam) :pointer
  (notifier_pointer :pointer)
  (status :pointer))

(cffi:defcfun ("updateNotifierAlarm" updateNotifierAlarm) :void
  (notifier_pointer :pointer)
  (triggerTime :pointer)
  (status :pointer))

(cffi:defcfun ("stopNotifierAlarm" stopNotifierAlarm) :void
  (notifier_pointer :pointer)
  (status :pointer))

;;; PDP
(cffi:defcfun ("initializePDP" initializePDP) :void
  (module :pointer))

(cffi:defcfun ("getPDPTemperature" getPDPTemperature) :double
  (module :pointer)
  (status :pointer))

(cffi:defcfun ("getPDPVoltage" getPDPVoltage) :double
  (module :pointer)
  (status :pointer))

(cffi:defcfun ("getPDPChannelCurrent" getPDPChannelCurrent) :double
  (module :pointer)
  (channel :pointer)
  (status :pointer))

(cffi:defcfun ("getPDPTotalCurrent" getPDPTotalCurrent) :double
  (module :pointer)
  (status :pointer))

(cffi:defcfun ("getPDPTotalPower" getPDPTotalPower) :double
  (module :pointer)
  (status :pointer))

(cffi:defcfun ("getPDPTotalEnergy" getPDPTotalEnergy) :double
  (module :pointer)
  (status :pointer))

(cffi:defcfun ("resetPDPTotalEnergy" resetPDPTotalEnergy) :void
  (module :pointer)
  (status :pointer))

(cffi:defcfun ("clearPDPStickyFaults" clearPDPStickyFaults) :void
  (module :pointer)
  (status :pointer))

;;; Power
(cffi:defcfun ("getVinVoltage" getVinVoltage) :float
  (status :pointer))

(cffi:defcfun ("getVinCurrent" getVinCurrent) :float
  (status :pointer))

(cffi:defcfun ("getUserVoltage6V" getUserVoltage6V) :float
  (status :pointer))

(cffi:defcfun ("getUserCurrent6V" getUserCurrent6V) :float
  (status :pointer))

(cffi:defcfun ("getUserActive6V" getUserActive6V) :pointer
  (status :pointer))

(cffi:defcfun ("getUserCurrentFaults6V" getUserCurrentFaults6V) :int
  (status :pointer))

(cffi:defcfun ("getUserVoltage5V" getUserVoltage5V) :float
  (status :pointer))

(cffi:defcfun ("getUserCurrent5V" getUserCurrent5V) :float
  (status :pointer))

(cffi:defcfun ("getUserActive5V" getUserActive5V) :pointer
  (status :pointer))

(cffi:defcfun ("getUserCurrentFaults5V" getUserCurrentFaults5V) :int
  (status :pointer))

(cffi:defcfun ("getUserVoltage3V3" getUserVoltage3V3) :float
  (status :pointer))

(cffi:defcfun ("getUserCurrent3V3" getUserCurrent3V3) :float
  (status :pointer))

(cffi:defcfun ("getUserActive3V3" getUserActive3V3) :pointer
  (status :pointer))

(cffi:defcfun ("getUserCurrentFaults3V3" getUserCurrentFaults3V3) :int
  (status :pointer))

;;; Solenoid
(cffi:defcfun ("initializeSolenoidPort" initializeSolenoidPort) :pointer
  (port_pointer :pointer)
  (status :pointer))

(cffi:defcfun ("freeSolenoidPort" freeSolenoidPort) :void
  (solenoid_port_pointer :pointer))

(cffi:defcfun ("checkSolenoidModule" checkSolenoidModule) :pointer
  (module :pointer))

(cffi:defcfun ("getSolenoid" getSolenoid) :pointer
  (solenoid_port_pointer :pointer)
  (status :pointer))

(cffi:defcfun ("getAllSolenoids" getAllSolenoids) :pointer
  (solenoid_port_pointer :pointer)
  (status :pointer))

(cffi:defcfun ("setSolenoid" setSolenoid) :void
  (solenoid_port_pointer :pointer)
  (value :pointer)
  (status :pointer))

(cffi:defcfun ("getPCMSolenoidBlackList" getPCMSolenoidBlackList) :int
  (solenoid_port_pointer :pointer)
  (status :pointer))

(cffi:defcfun ("getPCMSolenoidVoltageStickyFault" getPCMSolenoidVoltageStickyFault) :pointer
  (solenoid_port_pointer :pointer)
  (status :pointer))

(cffi:defcfun ("getPCMSolenoidVoltageFault" getPCMSolenoidVoltageFault) :pointer
  (solenoid_port_pointer :pointer)
  (status :pointer))

(cffi:defcfun ("clearAllPCMStickyFaults_sol" clearAllPCMStickyFaults_sol) :void
  (solenoid_port_pointer :pointer)
  (status :pointer))
