/*
 * node.js makes prototyping the WS server easier.
 * I'm choosing to get the client-side working from this and then building
 * out the Haskell back-end.
 */
const WebSocket = require('ws');
const randomWords = require('random-words');


const wss = new WebSocket.Server({ port: 8080 });


const Scores = {
    'alice-smith-foobar': 14,
    'alice-smith-test': 8
};


wss.on('connection', function(ws) {
    ws.on('message', function(message) {
        console.log(message);
        if (!Scores.hasOwnProperty(message)) {
            return;
        }

        Scores[message] += 1;
        console.log(Scores);
        ws.send(['AssignScore', message, Scores[message].toString()].join(' '));
    });

    const username = randomWords({ exactly: 3, join: '-' });
    Scores[username] = 0;
    ws.send('AssignUsername ' + username);
    Object.keys(Scores).forEach(function(key) {
        if (Scores[key] === 0) {
            return;
        }

        ws.send(['AssignScore', key, Scores[key].toString()].join(' '));
    });
});
