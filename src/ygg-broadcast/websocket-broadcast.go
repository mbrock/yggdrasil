package main

import (
	"net/http"
	"flag"
	"log"
	"code.google.com/p/go.net/websocket"
	"github.com/streadway/amqp"
)

type hub struct {
	connections map [*connection] bool
	register chan *connection
	unregister chan *connection
	broadcast chan string
}

var h = hub {
	register: make(chan *connection),
	unregister: make(chan *connection),
	broadcast: make(chan string),
	connections: make(map[*connection]bool),
}

func (h *hub) run() {
	log.Print("Yggdrasil WebSockets Broadcaster running")

	for {
		select {
		case c := <-h.register:
			h.connections[c] = true
			log.Print("New connection")
		case c := <-h.unregister:
			log.Print("Connection closed")
			delete(h.connections, c)
			close(c.send)
		case m := <-h.broadcast:
			log.Print("Broadcasting message: %v", m)
			for c := range h.connections {
				select {
				case c.send <- m:
				default:
					close(c.send)
					delete(h.connections, c)
				}
			}
		}
	}
}

func (h *hub) runAmqpListener() {
	connection, err := amqp.Dial("amqp://guest:guest@localhost/")
	if err != nil {
		log.Fatal("Couldn't dial AMQP server")
	}

	channel, err := connection.Channel()
	if err != nil {
		log.Fatal("Couldn't open AMQP channel")
	}
	
	err = channel.ExchangeDeclare(
		"yggdrasil",
		"direct",
		true,
		false,
		false,
		false,
		nil)
	if err != nil {
		log.Fatal("Couldn't open AMQP exchange")
	}

	state, err := channel.QueueDeclare(
		"event-broadcast",
		true,
		false,
		false,
		false,
		nil)
	if err != nil {
		log.Fatal("Couldn't open AMQP queue")
	}

	log.Printf("AMQP: %d messages, %d consumers",
		state.Messages, state.Consumers)

	err = channel.QueueBind(
		"event-broadcast",
		"event",
		"yggdrasil",
		false,
		nil)
	if err != nil {
		log.Fatal("Couldn't bind AMQP queue")
	}

	deliveries, err := channel.Consume(
		"event-broadcast",
		"websocket-broadcast",
		false,
		false,
		false,
		false,
		nil)
	if err != nil {
		log.Fatal("Couldn't open AMQP consumer")
	}

	log.Print("Connected to AMQP queue")

	go h.handle(deliveries)
}
	
func (h *hub) handle(deliveries <-chan amqp.Delivery) {
	for d := range deliveries {
		log.Printf("got %dB: [%v] %s",
			len(d.Body),
			d.DeliveryTag,
			d.Body)
		h.broadcast <- string(d.Body)
	}
}

var addr = flag.String("addr", ":8080", "http service address")

func main() {
	flag.Parse()
	go h.run()
	go h.runAmqpListener()
	http.Handle("/", websocket.Handler(wsHandler))
	if err := http.ListenAndServe(*addr, nil); err != nil {
		log.Fatal("ListenAndServe:", err)
	}
}
