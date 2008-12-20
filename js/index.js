/* test to see whether the js file loaded correctly */
function helloWorld() {
    alert("Hello world");
}

/* add observers when page finishes loading */
document.observe("dom:loaded", function() {
    /*$("header").observe("click", function(event) {
	alert(typeof(event));
    });*/
    console.log("Added all observers.");
});
