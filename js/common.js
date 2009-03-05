function logit(s) {
    if (window["console"] && console.log)
	console.log(s);
}

function setupDisqus() {
    logit("Inserting DISQUS code...");
    var links = document.getElementsByTagName('a');
    var query = '?';
    for (var i = 0; i < links.length; i++) {
	if (links[i].href.indexOf('#disqus_thread') >= 0) {
            query += 'url' + i + '=' + encodeURIComponent(links[i].href) + '&';
	}
    }
    document.body.insert(new Element("script", {
	charset: "utf-8"
	, type: "text/javascript"
	, src: "http://disqus.com/forums/scvalex/get_num_replies.js" + query
    }));
    logit("DISQUS code inserted");
    logit("http://disqus.com/forums/scvalex/get_num_replies.js" + query);
}

Event.observe(window, "load", function() {
    logit("Setting up page...");
    setupDisqus();
    logit("Page setup done.");
});

logit("Loaded common.js");
