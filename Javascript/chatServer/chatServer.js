var net = require('net');
var eol = require('os').EOL;

var srvr = net.createServer();
var clientList = [];

srvr.on('connection', function(client) {
  client.name = client.remoteAddress + ':' + client.remotePort;
  client.write('Welcome, ' + client.name + eol);
  clientList.push(client);

  client.on('data', function(data) {

    var command = data.toString().trim();

    if(command.startsWith('\\list')){
      for (var i in clientList){
        client.write(clientList[i].name + "\n");
      }
    }
    else if (command.startsWith('\\rename')){
      oldName = client.name;
      client.name = command.substring(command.indexOf(' ')+1).trim();
      for ( var i in clientList){
        clientList[i].write(oldName + " is now " + client.name + "\n");
      }
    }
    else if (command.startsWith('\\private')){
      comm = command.split(' ');

      for (var i in clientList){
        if (comm[1] == clientList[i].name){
          clientList[i].write(client.name + " privately messages "+ comm[2]);          
        }
      }
    }
    else{
      broadcast(data, client);
    }

  });

});

function broadcast(data, client) {
  for (var i in clientList) {
    if (client !== clientList[i]) {
      clientList[i].write(client.name + " says " + data);
    }
  }
}

srvr.listen(9000);
