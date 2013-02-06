$(function() {
	var rootUrl = "http://localhost/cgi-bin/cgi1";
	dataSource = new kendo.data.DataSource({
		pageSize: 10,
		serverPaging: true,
		transport: {
			read: rootUrl + "/person",
			create: {
				url: rootUrl + "/person",
				type: "POST"
			},
			update: {
				url: function(person) {
					return rootUrl + "/person/" + person.id
				},
				type: "PUT"
			},
			destroy: {
				url: function(person) {
					return rootUrl + "/person/" + person.id
				},
				type: "DELETE",
				complete: function(e) {
					$("#grid").data("kendoGrid").dataSource.read();
				}
			},
			parameterMap: function(options, operation) {
				if (operation === "create")
					delete options.id;
				return options;
			}
		},
		error: function(e) {
			alert(e.errorThrown);
		},
		schema: {
			data: "items",
			total: "count",
			model: {
				id: "id",
				fields: {
					name: { validation: { required: true } }
				}
			}
		}
	});

	$("#grid").kendoGrid({
		dataSource: dataSource,
		scrollable: true,
		sortable: true,
		filterable: true,
		selectable: true,
		height: 560,
		toolbar: ["create"],
		pageable: {
			refresh: true,
			pageSizes: true
		},
		save: function(e) {
			// update the id of the new records
			if (e.model.id == 0)
				e.model.set("id", this.dataSource.data().length);
		},
		columns: [
			{ field: "name", title: "Name" },
			{ title: "&nbsp;", width: "200px", command: [ "edit", "destroy" ] }
		],
		editable: "popup"
	});
});