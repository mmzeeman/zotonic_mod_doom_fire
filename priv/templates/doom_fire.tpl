<!DOCTYPE html>
<html lang="en">
    <head>
        {% all include "_html_head.tpl" %}
        <title>DOOM's Fire</title>

        {% lib "css/main.css" %}
    </head>
    <body>
        <h1>DOOM's fire</h1>

        <p>
        Made with Zotonic Teleview
        </p>

        {% teleview 
             type=`doom_fire`
             template="_the_doom_fire.tpl"
             width=60
             height=35
             tick=70

             vary=%{ }
        %}
    </body>

    {% include "_js_include.tpl" %}
    {% script %}
</html>
