function grid(params) {
	var addOptions = {
		afterComplete : function(response, postData, formId) {
			var res = response.responseText;
			if (res != "") {
				var obj = $.parseJSON(res);
				postData.id = obj.id;
				params.grid.setSelection(postData.id);
			}
		},
		serializeEditData: function(obj) {
			delete obj.oper;
			delete obj.id;
			return obj;
		},
		url: params.apiUrl + params.path,
		mtype: "POST",
		savekey: [true, 13],
		closeAfterEdit: true,
		closeOnEscape: true
	};
	var editOptions = {
		onclickSubmit: function(obj, data) {
			obj.url = params.apiUrl + params.path + "/" + data.grid_id;
		},
		afterComplete : function(response, postData, formId) {
			params.grid.setSelection(postData.id);
		},
		serializeEditData: function(obj) {
			delete obj.oper;
			// delete obj.id;
			return obj;
		},
		url: params.apiUrl + params.path + "/",
		mtype: "PUT",
		savekey: [true, 13],
		closeAfterEdit: true,
		closeOnEscape: true
	};
	var delOptions = {
		onclickSubmit: function(obj, id) {
			obj.url = params.apiUrl + params.path + "/" + id;
		},
		serializeDelData: function(obj) {
			return "";
		},
		url: params.apiUrl + params.path + "/",
		mtype: "DELETE",
		closeOnEscape: true
	};
	params.grid.jqGrid({
		ajaxGridOptions: {
			contentType: "application/json; charset=utf-8"
		},
		//cmTemplate: { sortable: false },
		prmNames: { filters: null, search: null, nd: null/*, order: null, sort: null*/ },
		colNames: [ "ID", "Name" ],
		colModel: [
			{ name: "id", index: "id", label: "ID", key: true, formatter: "integer", width: 4, editable: false, editoptions: { readonly: true, size: 5 } },
			{ name: "name", index: "name", labe: "Name", width: 50, editable: true, editrules: { required: true } }
		],
		ondblClickRow: function(id) {
			params.grid.jqGrid("editGridRow", id, editOptions);
		},
		rowList: [5, 10, 20],
		rowNum: 10,
		url: params.apiUrl + params.path,
		datatype: "json",
		width: 600,
		height: "auto",
		pager: "#pager",
		sortable: true,
		hidegrid: false,
		rownumbers: true,
		viewrecords: true,
		caption: "Person CRUD"
	}).navGrid("#pager", {
			add: true, edit: true, del: true, refresh: true, refreshstate: "current", view: false, search: false,
			addtext: "<span class='ui-pg-button-text'>Add</span>",
			edittext: "<span class='ui-pg-button-text'>Edit</span>",
			deltext: "<span class='ui-pg-button-text'>Delete</span>",
			refreshtext: "<span class='ui-pg-button-text'>Reload</span>"
		},
		editOptions,
		addOptions,
		delOptions
	);
	params.grid.jqGrid("bindKeys", {
		scrollingRows: true
	});
};