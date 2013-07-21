ko.extenders.numeric = function(target, precision) {
    var result = ko.computed({
        read: target,
        write: function(newValue) {
            var current = target(),
                roundingMultiplier = Math.pow(10, precision),
                newValueAsNum = isNaN(newValue) ? 0 : parseFloat(+newValue),
                valueToWrite = Math.round(newValueAsNum * roundingMultiplier) / roundingMultiplier;
 
            if (valueToWrite !== current) {
                target(valueToWrite);
            } else {
                if (newValue !== current) {
                    target.notifySubscribers(valueToWrite);
                }
            }
        }
    });
    result(target());
    return result;
};


var Channel = function(options) {
  this.options = options || {};

  this.url = this.options.host || 'ws://' + document.location.host + '/websocket';
  this.connected = false;
};

Channel.prototype = {
  connect: function() {
    this.socket = new WebSocket(this.url);

    this.socket.onopen    = this.onOpen.bind(this);
    this.socket.onclose   = this.onClose.bind(this);
    this.socket.onmessage = this.onMessage.bind(this);
    this.socket.onerror   = this.onError.bind(this);
  },

  disconnect: function() {
    this.socket.close();
  },

  send: function(message) {
    console.log(message);
    this.socket.send(JSON.stringify(message));
  },

  onOpen: function() {
    this.connected = true;
  },

  onClose: function() {
    console.log('Close', arguments);
  },

  onError: function() {
    console.log('Error', arguments);
  },

  onMessage: function(message) {
    response = JSON.parse(message.data);
    this.options.onMessage(response);
  }
};

var MetaModel = function(application, storage) {
  this.application = application;
  this.clients   = ko.observable(0);
  this.requests  = ko.observable(0);
  this.min_time  = ko.observable(0).extend({ numeric: 2 });
  this.max_time  = ko.observable(0).extend({ numeric: 2 });
  this.mean_time = ko.observable(0).extend({ numeric: 2 });

  this.min_time.subscribe(function(value) {
    storage.min.push(value);
  });
  this.max_time.subscribe(function(value) {
    storage.max.push(value);
  });
  this.mean_time.subscribe(function(value) {
    storage.arithmetic_mean.push(value);
  });

};
MetaModel.prototype = {
  reset: function() {
    this.clients(0);
    this.requests(0);
    this.min_time(0);
    this.max_time(0);
    this.mean_time(0);
  }
};

var Chart = function(storage) {
  this.storage = storage;
  this.context = cubism.context()
      .serverDelay(5000)
      .clientDelay(5000)
      .step(1e3)
      .size(960);

  this.min  = this.diagram("Min",  "min");
  this.max  = this.diagram("Max",  "max");
  this.mean = this.diagram("Mean", "arithmetic_mean");
};

Chart.prototype = {
  diagram: function(name, key) {
    var storage = this.storage;
    return this.context.metric(function(start, stop, step, callback) {
      start = +start;
      stop  = +stop;

      if(storage[key]) {
        storage[key] = storage[key].slice((start - stop) / step);
        callback(null, storage[key]);
      }
    }, name);
  },

  render: function() {
    d3.select(".diagram").call(function(div) {
      div.append("div")
         .attr("class", "axis")
         .call(this.context.axis().orient("top"));

      div.selectAll(".horizon")
         .data([this.min, this.max, this.mean])
         .enter()
         .append("div")
         .attr("class", "horizon")
         .call(this.context.horizon().extent([-20, 20]));

      div.append("div")
         .attr("class", "rule")
         .call(this.context.rule());
    }.bind(this));
  }
};

var Application = function(storage) {
  this.meta = new MetaModel(this, storage);

  this.channel = new Channel({
    onMessage: this.update.bind(this)
  });
  this.channel.connect();

  this.chart = new Chart(storage);
  this.chart.render();
};

Application.prototype = {
  update: function(message) {
    if(message.requests) {
      this.meta.requests(message.requests)
    }

    if(message.clients) {
      this.meta.clients(message.clients)
    }

    if(message.request_time) {
      this.meta.min_time(message.request_time.min / 1000)
      this.meta.max_time(message.request_time.max / 1000)
      this.meta.mean_time(message.request_time.arithmetic_mean / 1000)
    }
  },
  boom: function(){
    this.application.channel.send({ action: 'add_worker' });
  }
};



$(function(){
  var storage = {
    min: [],
    max: [],
    arithmetic_mean: []
  };

  var application = new Application(storage);
  ko.applyBindings(application);
});
