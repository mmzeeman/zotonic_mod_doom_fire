<table cellpadding="0" cellspacing="0" style="margin: 10px auto;">
    {% for y in 1|range:height %}
        <tr>
            {% for x in 1|range:width %} 
            <td class="pixel color-{{ m.doom_fire.pixel[x][y] }}"></td>
            {% endfor %}
        </tr>
    {% endfor %}
</table>
