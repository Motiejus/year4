configuration TinyBlogAppC { }
implementation
{
  components TinyBlogC, MainC, ActiveMessageC, LedsC,
    new TimerMilliC(),
    new AMReceiverC(AM_TINYBLOGMSG),
    new AMSenderC(AM_TINYBLOGMSG);

  TinyBlogC.Boot -> MainC;
  TinyBlogC.RadioControl -> ActiveMessageC;
  TinyBlogC.AMSend -> AMSenderC;
  TinyBlogC.Timer -> TimerMilliC;
  TinyBlogC.Leds -> LedsC;

  
}
