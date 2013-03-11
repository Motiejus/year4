#include "Timer.h"
#include "TinyBlogMsg.h"

module TinyBlogC @safe()
{
    uses {
        interface Boot;
        interface SplitControl as RadioControl;
        interface AMSend;
        interface Timer<TMilli>;
        interface Leds;
    }
}
implementation
{
    message_t sendBuf;
    bool sendBusy;

    // Use LEDs to report various status issues.
    void report_problem() { call Leds.led0Toggle(); }
    void report_sent() { call Leds.led1Toggle(); }
    void report_received() { call Leds.led2Toggle(); }

    event void Boot.booted() {
        if (call RadioControl.start() != SUCCESS)
            report_problem();
    }

    void startTimer() {
        call Timer.startPeriodic(7400);
    }

    event void RadioControl.startDone(error_t error) {
        startTimer();
    }

    event void RadioControl.stopDone(error_t error) {
    }


    event void Timer.fired() {
        tinyblog_t blogmsg;
        blogmsg.seqno = 7;
        blogmsg.sourceMoteID = TOS_NODE_ID;
        blogmsg.destMoteID = 0;
        blogmsg.action = POST_TWEET;
        blogmsg.hopCount = 0;
        blogmsg.nchars = 5;
        memcpy(blogmsg.data, "aaaa\04", 5);
        blogmsg.mood = 0;

        if (!sendBusy && sizeof blogmsg <= call AMSend.maxPayloadLength()) {
            // Don't need to check for null because we've already checked length
            // above
            memcpy(call AMSend.getPayload(&sendBuf, sizeof(blogmsg)),
                    &blogmsg, sizeof blogmsg);
            if (call AMSend.send(AM_BROADCAST_ADDR, &sendBuf,
                        sizeof blogmsg) == SUCCESS)
                sendBusy = TRUE;
        }

        if (!sendBusy)
            report_problem();
    }

    event void AMSend.sendDone(message_t* msg, error_t error) {
        if (error == SUCCESS)
            report_sent();
        else
            report_problem();

        sendBusy = FALSE;
    }

}
