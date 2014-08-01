var editor1 = document.querySelector("#editor1");
var editor2 = document.querySelector("#editor2");

var hanoiSample =
    "def hanoi(ndisks, startPeg, endPeg):\n" +
    "   if ndisks:\n" +
    "       hanoi(ndisks-1, startPeg, 6-startPeg-endPeg)\n" +
    "       print(startPeg, endPeg)\n" +
    "       hanoi(ndisks-1, 6-startPeg-endPeg, endPeg)\n\n" +
    "hanoi(4, 1, 3)";

var editor = CodeMirror(editor1, {
    value: hanoiSample,
    lineNumbers: true,
    lineWrapping: true,
    foldGutter: {
    },
    mode:  "python"
});

var result = CodeMirror(editor2, {
    value: "",
    lineNumbers: true,
    readOnly: true,
    mode: "shell"
});

var compile = function(includeStdlib){
    var compiledString = "";
    if (includeStdlib) {
        compiledString += shlystdlib().bytes + "\n";
    }
    try {
        compiled = shlycompile(editor.getValue());
        compiledString += compiled.bytes;
    } catch (e){
        console.log(e);
        var error = e[1][1].bytes;
        if (e[2] && e[2].bytes){
            error += ": " + e[2].bytes;
        }

        compiledString = error;
    }

    result.setValue(compiledString);
};

Zepto(function($){
    compile();
    $("#compile-button").click(function(){
        compile(false);
    });

    $("#compile-std-button").click(function(){
        compile(true);
    });
});
