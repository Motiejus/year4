configuration TinyBlogAppC { }
implementation
{
  components TinyBlogC, MainC, ActiveMessageC, LedsC,
    new AMReceiverC(AM_TINYBLOGMSG),
    new AMSenderC(AM_TINYBLOGMSG),
    new SensirionSht11C(),
    new TimerMilliC() as Timer_send,
    new TimerMilliC() as Timer_sense;

  TinyBlogC.Boot -> MainC;
  TinyBlogC.RadioControl -> ActiveMessageC;
  TinyBlogC.AMSend -> AMSenderC;
  TinyBlogC.Read -> SensirionSht11C.Temperature;
  TinyBlogC.Leds -> LedsC;

  TinyBlogC.Timer_send -> Timer_send;
  TinyBlogC.Timer_sense -> Timer_sense;

  
}
