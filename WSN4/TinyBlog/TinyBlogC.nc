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
} circular_t;


module TinyBlogC @safe()
{
    uses {
        interface Boot;
        interface SplitControl as RadioControl;
        interface Receive;
        interface AMSend;
        interface Timer<TMilli> as Timer_send;
        interface Timer<TMilli> as Timer_sense;
        interface Read<uint16_t>;
        interface Leds;
    }
}
implementation
{
    message_t sendBuf;
    tinyblog_t blogmsg_out;
    uint16_t mood;

    /* Circular tweeter stuff */
    circular_t tweets_circ[STORE_TWEETS];
    circular_t *head, /* Next available slot */
               *tail; /* First message to be sent out. If == head, none. */
    nx_uint16_t base_station;
    bool sendBusy;

    nx_uint8_t followers[MAX_FOLLOWERS];
    nx_uint8_t num_followers;

    void report_problem() { call Leds.led0Toggle(); } /* Red */
    void report_sent() { call Leds.led1Toggle(); } /* Green */
    void report_received() { call Leds.led2Toggle(); } /* Blue */

    /***************************************************************************
     * Helpers
     **************************************************************************/
    void save_tweet(tinyblog_t *blogmsg) {
        dbg("All", "saving tweet\n");
        atomic {
            if (tail == head) {
                /* Bad fortune, overwriting a tweet. */
                if (tail->nchars >= 1)
                    tail->data[tail->nchars-1] = '\0';
                else
                    tail->data[0] = '\0';
                dbg("All", "overwriting tweet data: '%s'\n",
                        tail->data);
                tail = tail->next;
            }
            memset(head->data, 0, DATA_SIZE);
            memcpy(head->data, blogmsg->data, blogmsg->nchars);
            head->nchars = blogmsg->nchars;
            head->from = blogmsg->sourceMoteID;
            head = head->next;
        }
    }

    void get_tweets(nx_uint16_t station) {
        base_station = station;
        call Timer_send.startOneShot(0);
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
        }
        tweets_circ[STORE_TWEETS-1].next = &tweets_circ[0];
        tail = head = tweets_circ;
        num_followers = 0;
    }


    /***************************************************************************
     * TinyOS callbacks
     **************************************************************************/
    event void Boot.booted() {
        initialize_structures();
        if (call RadioControl.start() != SUCCESS)
            report_problem();

        call Timer_sense.startPeriodic(5000);
        dbg("All", "Application booted.\n");
    }

    event void RadioControl.startDone(error_t error) { }

    event void RadioControl.stopDone(error_t error) { }

    event void AMSend.sendDone(message_t* msg, error_t error) {
        if (error == SUCCESS)
            report_sent();
        else
            report_problem();

        sendBusy = FALSE;
    }

    event message_t* Receive.receive(message_t* bufPtr, 
            void* payload, uint8_t len) {
        dbg("All", "Received payload of size %d.\n", len);
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
                    /* Host mote requests all tweets, send them out */
                    get_tweets(blogmsg->sourceMoteID);
                }
            }
            return bufPtr;
        }
    }

    event void Timer_sense.fired() {
        call Read.read();
    }

    /* This means there can be some tweets in the buffer */
    event void Timer_send.fired() {
        /* If head == tail, then buffer is empty */
        if (head != tail) {
            if (sendBusy) {
                /* Retry after 10ms */
                call Timer_send.startOneShot(10);
            } else if (!sendBusy &&
                    sizeof blogmsg_out <= call AMSend.maxPayloadLength()) {
                dbg("All", "Sending out tweet of length %d\n",
                        tail->nchars);
                memcpy(call AMSend.getPayload(&sendBuf,
                            sizeof(blogmsg_out)), &blogmsg_out,
                        sizeof (blogmsg_out));
                if (call AMSend.send(AM_BROADCAST_ADDR, &sendBuf,
                            sizeof blogmsg_out) == SUCCESS)
                    sendBusy = TRUE;

                atomic {
                    blogmsg_out.seqno = 0; /* TODO */
                    blogmsg_out.sourceMoteID = TOS_NODE_ID;
                    blogmsg_out.destMoteID = base_station;
                    blogmsg_out.action = GET_TWEETS;
                    blogmsg_out.hopCount = 0;
                    blogmsg_out.nchars = tail->nchars;
                    memcpy(blogmsg_out.data, tail->data, tail->nchars);
                    blogmsg_out.mood = mood;
                    tail = tail->next;
                }
                /* There can be more stuff in the queue */
                call Timer_send.startOneShot(0);
            }
            if (!sendBusy)
                report_problem();
        }
    }

    event void Read.readDone(error_t result, uint16_t data) {
        if (result == SUCCESS) {
            mood = data;
            dbg("All", "sensed %d\n", mood);
        }
    }
}
