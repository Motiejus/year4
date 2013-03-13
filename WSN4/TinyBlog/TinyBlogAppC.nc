configuration TinyBlogAppC { }
implementation
{
  components TinyBlogC, MainC, ActiveMessageC, LedsC,
    new AMReceiverC(AM_TINYBLOGMSG),
    new AMSenderC(AM_TINYBLOGMSG),
#ifdef TOSSIM
    new DemoSensorC(),
#else SIM
    new SensirionSht11C(),
#endif
    new TimerMilliC() as Timer_send,
    new TimerMilliC() as Timer_sense;

  TinyBlogC.Boot -> MainC;
  TinyBlogC.RadioControl -> ActiveMessageC;
  TinyBlogC.AMSend -> AMSenderC;
#ifdef TOSSIM
  TinyBlogC.Read -> DemoSensorC;
#else
  TinyBlogC.Read -> SensirionSht11C.Temperature;
#endif
  TinyBlogC.Leds -> LedsC;

  TinyBlogC.Timer_send -> Timer_send;
  TinyBlogC.Timer_sense -> Timer_sense;

  
}
