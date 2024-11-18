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
             width=100
             height=50

             tick=100
             keyframe_min_time=10000
             keyframe_max_time=30000
        %}
    </body>

    {% include "_js_include.tpl" %}
    {% script %}
</html>
