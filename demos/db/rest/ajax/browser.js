$(function() {
	var result = $("#result");
	result.html("");
	$("#submit").on("click", function() {
		$.ajax({
			type: $("#method").val(),
			url: $("#url").val(),
			data: "name=" + $("#name").val(),
			success: function(data, textStatus, jqXHR) {
				switch (jqXHR.status) {
					case 200:
						result.html(jqXHR.responseText);
						break;
					case 201:
						result.html("Created.");
						break;
					case 204:
						result.html("No content.");
						break;
				}
			},
			error: function(jqXHR, textStatus, errorThrown) {
				if (jqXHR.status == 404)
					result.html("Not found.");
				else
					result.html(jqXHR.responseText);
			}
		});
	});
});