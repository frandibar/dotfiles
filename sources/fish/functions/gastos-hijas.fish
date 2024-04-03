function gastos-hijas
    cd ~/Sync/ledger
    ledger bal expenses:hijas expenses:salud:obra-social --period 'last month' --cleared
    cd -
end
