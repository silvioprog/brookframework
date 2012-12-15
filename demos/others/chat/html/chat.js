$(function() {
	var root = "/cgi-bin/cgi1";

	// if user submits the form
	$("#submitmsg").on("click", function() {
		var clientmsg = $("#usermsg").val();
		if (clientmsg.trim() != "") {
			$.post(root + "/msg/post", { text: clientmsg });
			$("#usermsg").val("");
		}
		return false;
	});

	// load the file containing the chat log
	function loadLog() {
		var oldScrollHeight = $("#chatbox").prop("scrollHeight") - 20;
		$.ajax({
			url: root + "/msg",
			cache: false,
			success: function(html) {
				$("#chatbox").html(html); // insert chat log into the #chatbox div
				var newScrollHeight = $("#chatbox").prop("scrollHeight") - 20;
				if (newScrollHeight > oldScrollHeight)
					$("#chatbox").animate({ scrollTop: newScrollHeight }, "normal"); // autoscroll to bottom of div
			}
		});
	}
	setInterval (loadLog, 1500); // reload file every 2.5 seconds

	// if user wants to end session
	$("#exit").on("click", function() {
		var exit = confirm("Are you sure you want to end the session?");
		if (exit)
			window.location = root + "/logout";
		return false;
	});
});