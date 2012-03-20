Example template:

    {template hello name}
      Hello, {name}!
    {/template}

Haskell output:

    hello name
      = Data.Text.concat
          [Data.Text.pack "Hello, ", Text.Gogh.SafeShow.safeShow name,
           Data.Text.pack "!\n"]

JavaScript output (Template is the file name)

    Template.hello = function (name) {
        var _builder = new StringBuilder();
        _builder.append("Hello, ");
        _builder.append(name.safeShow());
        _builder.append("!\n");
        return _builder.toString();
    };