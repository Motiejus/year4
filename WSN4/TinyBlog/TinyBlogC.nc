#include "Timer.h"
#include "TinyBlogMsg.h"

#define STORE_TWEETS 8

typedef struct circ_t {
    char data[DATA_SIZE];
    short nchars;
    nx_uint16_t from; /* SourceMoteID */
    struct circ_t *next;
    bool nonempty;
} circular_t;


module TinyBlogC @safe()
{
    uses {
        interface Boot;
        interface SplitControl as RadioControl;
        interface Receive;
        interface AMSend;
        interface Timer<TMilli>;
        interface Leds;
    }
}
implementation
{
    message_t sendBuf;
    circular_t tweets_circ[STORE_TWEETS];
    circular_t* avail; /* Next available slot */
    bool sendBusy;

    // Use LEDs to report various status issues.
    void report_problem() { call Leds.led0Toggle(); }
    void report_sent() { call Leds.led1Toggle(); }
    void report_received() { call Leds.led2Toggle(); }

    void save_tweet(tinyblog_t *blogmsg) {
        memcpy(avail->data, blogmsg->data, blogmsg->nchars);
        avail->nchars = blogmsg->nchars;
        avail->from = blogmsg->sourceMoteID;
        avail->nonempty = TRUE;
        avail = avail->next;
    }

    void get_tweets() {
    }

    event void Boot.booted() {
        int i;
        for (i = 0; i < STORE_TWEETS - 1; i++) {
            tweets_circ[i].next = &tweets_circ[i+1];
            tweets_circ[i].nonempty = FALSE;
        }
        tweets_circ[STORE_TWEETS-1].next = &tweets_circ[0];
        tweets_circ[STORE_TWEETS-1].nonempty = FALSE;
        avail = tweets_circ;

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
            // Don't need to check for null because we've already checked
            // length above
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

    event message_t* Receive.receive(message_t* bufPtr, 
            void* payload, uint8_t len) {
        report_received();

        if (len != sizeof(tinyblog_t)) {
            return bufPtr;
        } else {
            tinyblog_t* blogmsg = (tinyblog_t*)payload;

            if (blogmsg->destMoteID == TOS_NODE_ID) {
                if (blogmsg->action == POST_TWEET) {
                    /* Scenario 1, somebody posted a tweet to *this* mote,
                     * time to store it */
                    save_tweet(blogmsg);
                } else if (blogmsg->action == GET_TWEETS) {
                    /* Host mote requests all tweets, send them */
                    get_tweets();
                }
            }
            return bufPtr;
        }
    }

}
