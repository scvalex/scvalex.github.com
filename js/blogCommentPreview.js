var BlogCommentPreview = {
    updateLivePreview: function() {
	var commentText = $("comment_text").value.stripTags();
	$("live_preview").update(commentText);
    }
};

Event.observe(window, "dom:loaded", function() {
    $("comment_text").observe("keyup", BlogCommentPreview.updateLivePreview);
});
