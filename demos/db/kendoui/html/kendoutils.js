function refresh(grid) {
	grid.data("kendoGrid").dataSource.read();
}

function customize(grid) {
	$(".k-grid-edit").on("click", function() {
		event.preventDefault();
		var dt = grid.data("kendoGrid");
		var select = dt.select();
		var row = dt.dataItem(select);
		if (row !== null)
			dt.editRow(select);
	});

	$(".k-grid-delete").on("click", function() {
		event.preventDefault();
		var dt = grid.data("kendoGrid");
		var select = dt.select();
		var row = dt.dataItem(select);
		if (row !== null)
			if (confirm("Delete record?"))
				dt.removeRow(select);
	});
}

function grid(params) {
	params.grid.kendoGrid({
		dataSource: {
			transport: {
				read: {
					url: params.apiUrl + params.path,
					type: "GET",
					dataType: "json"
				},
				create: {
					complete: function(obj) {
						refresh(grid);
					},
					url: params.apiUrl + params.path,
					type: "POST",
					dataType: "json"
				},
				update: {
					url: function(obj) {
						return params.apiUrl + params.path + "/" + obj.id;
					},
					complete: function(obj) {
						refresh(grid);
					},
					type: "PUT",
					dataType: "json"
				},
				destroy: {
					url: function(obj) {
						return params.apiUrl + params.path + "/" + obj.id;
					},
					complete: function(obj) {
						refresh(grid);
					},
					type: "DELETE",
					dataType: "json"
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
					fields: params.fields
				}
			},
			pageSize: 10,
			serverPaging: true,
		},
		editable: {
			window: {
				title: "Edit record"
			},
			mode: "popup",
			confirmation: false
		},
		toolbar: [
			{ name: "create", text: "Insert" },
			{ name: "edit", text: "Edit" },
			{ name: "destroy", text: "Delete" }
		],
		pageable: {
			refresh: true,
			pageSizes: true
		},
		edit: function (obj) {
			$(".k-grid-update").html("<span class='k-icon k-update'></span>OK");
			$(".k-grid-cancel").html("<span class='k-icon k-cancel'></span>Cancel");
		},
		columns: params.columns,
		height: 400,
		scrollable: true,
		selectable: true
	});
	customize(params.grid);
}