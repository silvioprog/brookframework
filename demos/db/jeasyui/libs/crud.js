$(function(){
	var rootURL = "/cgi-bin/person";
	var url;

	$("#main").show();

	$("#fm").submit(function () {
		return false;
	});

	function addPerson() {
		$("#dlg").dialog("open").dialog("setTitle", "Add Person");
		$("#fm").form("clear");
		url = rootURL + "/add";
	}

	function editPerson(){
		var row = $("#dg").datagrid("getSelected");
		if (row){
			$("#dlg").dialog("open").dialog("setTitle", "Edit Person");
			$("#fm").form("load", row);
			url = rootURL + "/edit/" + row.id;
		}
	}

	function deletePerson(){
		var row = $("#dg").datagrid("getSelected");
		if (row) {
			$.messager.confirm("Confirm", "Delete person?", function(r) {
				if (r){
					$.post(rootURL + "/delete", { id: row.id }, function(result) {
						if (result.success){
							$("#dg").datagrid("reload");
						} else {
							$.messager.show({
								title: "Error",
								msg: result.msg
							});
						}
					}, "json");
				}
			});
		}
	}

	function savePerson() {
		$("#fm").form("submit", {
			url: url,
			onSubmit: function() {
				return $(this).form("validate");
			},
			success: function(result) {
				var result = eval("(" + result + ")");
				if (result.success) {
					$("#dlg").dialog("close");
					$("#dg").datagrid("reload");
				} else {
					$.messager.show({
						title: "Error",
						msg: result.msg
					});
				}
			}
		});
	}

	$("#dg").datagrid({
		rownumbers: true,
		pagination: true,
		singleSelect: true,
		width: 500,
		height: 250,
		remoteSort: false,
		url: rootURL + "/list",
		toolbar: [
			{
				text: "Add",
				iconCls: "icon-add",
				handler: function() {
					addPerson();
				}
			},
			"-",
			{
				text: "Edit",
				iconCls: "icon-edit",
				handler: function() {
					editPerson();
				}
			},
			"-",
			{
				text: "Delete",
				iconCls: "icon-remove",
				handler: function() {
					deletePerson();
				}
			}
		],
		columns:[[
			{ field: "id", title: "ID", width: 50, align: "right", sortable: true },
			{ field: "name", title: "Person", width: 380, sortable: true }
		]]
	});

	$("#dlg").dialog({
		closed: true,
		buttons: [
			{
				text: "OK",
				iconCls: "icon-ok",
				handler: function() {
					savePerson();
				}
			},
			{
				text: "Cancel",
				iconCls: "icon-cancel",
				handler: function() {
					$("#dlg").dialog("close");
				}
			},
		]
	});
});