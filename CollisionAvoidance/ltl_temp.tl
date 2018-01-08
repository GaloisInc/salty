controller {{controller_name}} where

{{types}}

{{inputs}}

{{outputs}}

env_liveness
{% for env_tran in env_trans%}
{{env_tran}}
{% endfor %}

sys_trans
{%for sys_tran in sys_trans%}
{{sys_tran}}
{% endfor %}

{% for spec in sys_liveness %}
sys_liveness
{{spec}}
{% endfor %}
