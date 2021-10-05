<!DOCTYPE html>
<html lang="en">
    <head>
    </head>
    <body>
        <h1>DOOM's fire</h1>

        <p>
        Made with Zotonic Teleview
        </p>

        {# include "_the_doom_fire.tpl" width=60 height=35 #}

        {% teleview 
             type=`doom_fire`
             template="_the_doom_fire.tpl"
             topic="model/doom_fire/event/tick"
             width=60
             height=35
             tick=1000

             vary=%{ }
        %}
    </body>

    {% include "_js_include.tpl" %}
    {% script %}
</html>
