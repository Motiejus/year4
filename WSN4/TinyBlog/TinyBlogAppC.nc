configuration TinyBlogAppC { }
implementation
{
  components TinyBlogC, MainC, ActiveMessageC, LedsC,
    new TimerMilliC(),
    new AMSenderC(AM_TINYBLOGMSG);

  TinyBlogC.Boot -> MainC;
  TinyBlogC.RadioControl -> ActiveMessageC;
  TinyBlogC.AMSend -> AMSenderC;
  TinyBlogC.Timer -> TimerMilliC;
  TinyBlogC.Leds -> LedsC;

  
}
