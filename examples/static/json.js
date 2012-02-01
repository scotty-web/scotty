$(document).ready(function() {
    alert("here!");

    $("#fooquux").click(function () {
        $(".barfields").prop("disabled", true);
        $(".bazfields").prop("disabled", true);
    });

    $("#foobar").click(function () {
        $(".barfields").prop("disabled", false);
        $(".bazfields").prop("disabled", true);
    });

    $("#foobaz").click(function () {
        $(".bazfields").prop("disabled", false);
        $(".barfields").prop("disabled", true);
    });

    // Some things to note:
    // The result coming back (res) is a javascript object, so we must turn it to a string to view it.
    // Errors will fail silently, since we haven't declared an error handler.
    // JSON.stringify is broke-sauce... use $.toJSON from the jquery-json plugin.
    $("#fooform").submit(function () {
        var con = $(this).children('[name="con"]:checked').val();
        var inputs = $(this).children('[name^="' + con + '"]');
        var fields = $.map(inputs, function(v) { return $(v).val(); });
        $.ajax({ url: "/foo",
                 type: "POST",
                 data: mkCon(con,fields),
                 contentType: "application/json; charset=utf-8",
                 success: function(res) {
                              $("#foolog").append($.toJSON(res) + "</br>");
                          },
                 dataType: "json"}); // desired response type
        return false; // prevent default submission action
    });
});

function mkCon(con, fields) {
    // All user input is a string at first,
    // and we need to recover the numeric types
    if (con == 'Bar') {
        fields[0] = parseInt(fields[0]);
    } else if (con == 'Baz') {
        fields[0] = parseFloat(fields[0]);
    }

    // now build our object
    // Aeson seems to be inconsistent here (maybe it's the JSON spec?)
    // Constructors with zero or more than one fields become lists,
    // but Constructors with exactly on field become values.
    var o = {};
    if (fields.length == 1) {
        o[con] = fields[0];
    } else {
        o[con] = fields;
    }
    return $.toJSON(o);
}
