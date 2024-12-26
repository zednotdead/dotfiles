function kubedb
  if set -q "$argv[1]"
    echo "You need to provide a name for the database"
    return
  end

  set database "$argv[1]"

  if set -q $argv[2]
    set namespace "$argv[2]"
  else
    set namespace "database"
  end

  if set -q $argv[3]
    set host "$argv[3]"
  else
    set host "postgres.zed.gay"
  end

  # echo "$database"
  # echo "$namespace"

  set db_data $(kubectl get secrets -n $namespace -o=json postgres-pguser-$database | jq -r '.data | map_values(@base64d)')
  set username $(echo $db_data | jq .user)
  set password $(echo $db_data | jq .password)

  eval "PGPASSWORD=$password psql -U $username -h $host $database"
end
