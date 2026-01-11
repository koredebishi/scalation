$p = 'C:\Simulation\scalation_2.0\log\outfiles\spsa\spsa_41856611.out'
$lines = Get-Content -LiteralPath $p

$best = [double]::PositiveInfinity
$bestIdx = -1
$found = 0

for ($i = 0; $i -lt $lines.Count; $i++) {
  if ($lines[$i] -match 'Fitness value:\s*([-0-9.]+)') {
    $found++
    $v = [double]$matches[1]
    if ($v -lt $best) { $best = $v; $bestIdx = $i }
  }
}

Write-Host ("lines=$($lines.Count) fitnessEntries=$found")

if ($bestIdx -lt 0) {
  Write-Host 'No Fitness value lines found.'
  exit 1
}

$evalIdx = -1
for ($j = $bestIdx; $j -ge 0; $j--) {
  if ($lines[$j] -match '^Evaluating parameters:\s*VectorD\(') { $evalIdx = $j; break }
}

Write-Host ("BEST_FITNESS=$best")
Write-Host ("bestFitnessLineIndex=$bestIdx")
Write-Host ("bestFitnessLine=$($lines[$bestIdx])")
Write-Host ("evalLineIndex=$evalIdx")
Write-Host ("evalLine=$($lines[$evalIdx])")
