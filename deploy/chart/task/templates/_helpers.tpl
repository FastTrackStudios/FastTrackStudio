{{/* Chart name. */}}
{{- define "task.name" -}}
{{- .Chart.Name | trunc 63 | trimSuffix "-" -}}
{{- end -}}

{{/* Release-qualified name, the usual fullname convention. */}}
{{- define "task.fullname" -}}
{{- if contains .Chart.Name .Release.Name -}}
{{- .Release.Name | trunc 63 | trimSuffix "-" -}}
{{- else -}}
{{- printf "%s-%s" .Release.Name .Chart.Name | trunc 63 | trimSuffix "-" -}}
{{- end -}}
{{- end -}}

{{/* Common labels. */}}
{{- define "task.labels" -}}
helm.sh/chart: {{ printf "%s-%s" .Chart.Name .Chart.Version }}
app.kubernetes.io/name: {{ include "task.name" . }}
app.kubernetes.io/instance: {{ .Release.Name }}
app.kubernetes.io/version: {{ .Chart.AppVersion | quote }}
app.kubernetes.io/managed-by: {{ .Release.Service }}
{{- end -}}

{{/* Selector labels for a component; call with (dict "ctx" . "component" "server") */}}
{{- define "task.selectorLabels" -}}
app.kubernetes.io/name: {{ include "task.name" .ctx }}
app.kubernetes.io/instance: {{ .ctx.Release.Name }}
app.kubernetes.io/component: {{ .component }}
{{- end -}}

{{/* Image ref for a component; call with (dict "ctx" . "img" .Values.server.image) */}}
{{- define "task.image" -}}
{{- $tag := .img.tag | default .ctx.Chart.AppVersion -}}
{{- printf "%s/%s:%s" .ctx.Values.image.registry .img.repository $tag -}}
{{- end -}}

{{/* The server's public base URL (for signed attachment URLs). */}}
{{- define "task.publicUrl" -}}
{{- if .Values.server.publicUrl -}}
{{- .Values.server.publicUrl -}}
{{- else if .Values.ingress.enabled -}}
{{- printf "https://%s" .Values.ingress.host -}}
{{- end -}}
{{- end -}}
