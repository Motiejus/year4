#include "Timer.h"
#include "TinyBlogMsg.h"

#define STORE_TWEETS 8
#define MAX_FOLLOWERS 0x7f

/* Circular structure to store intermediate tweets */
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
    tinyblog_t blogmsg_out;
    circular_t tweets_circ[STORE_TWEETS];
    circular_t* avail; /* Next available slot */
    bool sendBusy;

    nx_uint8_t followers[MAX_FOLLOWERS];
    nx_uint8_t num_followers;

    void report_problem() { call Leds.led0Toggle(); }
    void report_sent() { call Leds.led1Toggle(); }
    void report_received() { call Leds.led2Toggle(); }

    /***************************************************************************
     * Helpers
     **************************************************************************/
    void save_tweet(tinyblog_t *blogmsg) {
        memset(avail->data, 0, DATA_SIZE);
        memcpy(avail->data, blogmsg->data, blogmsg->nchars);
        avail->nchars = blogmsg->nchars;
        avail->from = blogmsg->sourceMoteID;
        avail->nonempty = TRUE;
        avail = avail->next;
    }

    void get_tweets(nx_uint16_t base_station) {
        circular_t *old_avail;
        int i = 0;
        for (old_avail = avail->next; old_avail != avail; avail = avail->next) {
            i++;
            if (avail->nonempty) {
                blogmsg_out.seqno = i;
                blogmsg_out.sourceMoteID = TOS_NODE_ID;
                blogmsg_out.destMoteID = base_station;
                blogmsg_out.action = GET_TWEETS;
                blogmsg_out.hopCount = 0;
                blogmsg_out.nchars = avail->nchars;
                memcpy(blogmsg_out.data, avail->data, avail->nchars);
                blogmsg_out.mood = 0;

                if (!sendBusy &&
                        sizeof blogmsg_out <= call AMSend.maxPayloadLength()) {
                    memcpy(call AMSend.getPayload(&sendBuf,
                                sizeof(blogmsg_out)),
                            &blogmsg_out, sizeof (blogmsg_out));
                    if (call AMSend.send(AM_BROADCAST_ADDR, &sendBuf,
                                sizeof blogmsg_out) == SUCCESS)
                        sendBusy = TRUE;
                }
                if (!sendBusy)
                    report_problem();
            }
        }
    }

    bool is_follower(nx_uint8_t id) {
        int i;
        for (i = 0; i < num_followers; i++)
            if (followers[i] == id)
                return TRUE;
        return FALSE;
    }

    void initialize_structures() {
        int i;
        for (i = 0; i < STORE_TWEETS - 1; i++) {
            tweets_circ[i].next = &tweets_circ[i+1];
            tweets_circ[i].nonempty = FALSE;
        }
        tweets_circ[STORE_TWEETS-1].next = &tweets_circ[0];
        tweets_circ[STORE_TWEETS-1].nonempty = FALSE;
        avail = tweets_circ;
        num_followers = 0;
    }


    /***************************************************************************
     * TinyOS callbacks
     **************************************************************************/
    event void Boot.booted() {
        initialize_structures();
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
                    if (is_follower(blogmsg->sourceMoteID))
                        save_tweet(blogmsg);
                } else if (blogmsg->action == ADD_USER) {
                    if (!is_follower(blogmsg->data[0]))
                        if (num_followers < MAX_FOLLOWERS - 1)
                            followers[num_followers++] = blogmsg->data[0];
                        else
                            report_problem();
                } else if (blogmsg->action == GET_TWEETS) {
                    /* Host mote requests all tweets, send them */
                    get_tweets(blogmsg->sourceMoteID);
                }
            }
            return bufPtr;
        }
    }

 }
