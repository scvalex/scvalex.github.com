function logit(s) {
    if (window["console"] && console.log)
	console.log(s);
}

document.observe("dom:loaded", function() {
    logit("Added all observers.");
});
