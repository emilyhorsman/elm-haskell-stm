/*
 * node.js makes prototyping the WS server easier.
 * I'm choosing to get the client-side working from this and then building
 * out the Haskell back-end.
 */
const WebSocket = require('ws');
const randomWords = require('random-words');


const wss = new WebSocket.Server({ port: 8080 });


const Scores = {
    'alice-smith-foobar': { score: 14, time: +(new Date()) },
    'alice-smith-test': { score: 8, time: +(new Date()) }
};


wss.on('connection', function(ws) {
    ws.on('message', function(message) {
        console.log(message);
        if (!Scores.hasOwnProperty(message)) {
            return;
        }

        Scores[message].score += 1;
        Scores[message].time = +(new Date());
        console.log(Scores);
        ws.send([
            'AssignScore',
            message,
            Scores[message].score.toString(),
            Scores[message].time.toString()
        ].join(' '));
    });

    const username = randomWords({ exactly: 3, join: '-' });
    Scores[username] = { score: 0, time: +(new Date()) };
    ws.send('AssignUsername ' + username);
    Object.keys(Scores).forEach(function(key) {
        if (Scores[key].score === 0) {
            return;
        }

        ws.send([
            'AssignScore',
            key,
            Scores[key].score.toString(),
            Scores[key].time.toString()
        ].join(' '));
    });
});
