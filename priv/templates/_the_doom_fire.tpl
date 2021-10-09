<table cellpadding="0" cellspacing="0" style="margin: 10px auto;">
    {% for row in m.doom_fire.rows[doom_fire] %}
        <tr>
            {% for intensity in row %} 
                <td class="pixel color_{{ intensity }}"></td>
            {% endfor %}
        </tr>
    {% endfor %}
</table>
