function logit(s) {
    if (window["console"] && console.log)
	console.log(s);
}

function setupDisqus() {
    var links = document.getElementsByTagName('a');
    var query = '?';
    for (var i = 0; i < links.length; i++) {
	if(links[i].href.indexOf('#disqus_thread') >= 0) {
            query += 'url' + i + '=' + encodeURIComponent(links[i].href) + '&';
	}
    }
    document.insert(new Element("script", {
	charset: "utf-8"
	, type: "text/javascript"
	, src: "http://disqus.com/forums/scvalex/get_num_replies.js" + query
    }));
    logit("DISQUS code inserted");
    logit("http://disqus.com/forums/scvalex/get_num_replies.js" + query);
}

document.observe("dom:load", function() {
    setupDisqus();
});
