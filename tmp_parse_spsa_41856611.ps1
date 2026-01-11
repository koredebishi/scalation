$p = 'C:\Simulation\scalation_2.0\log\outfiles\spsa\spsa_41856611.out'
$lines = Get-Content -LiteralPath $p

$best = [double]::PositiveInfinity
$bestIdx = -1

for ($i = 0; $i -lt $lines.Count; $i++) {
  if ($lines[$i] -match 'Fitness value:\s*([-0-9.]+)') {
    $v = [double]$matches[1]
    if ($v -lt $best) {
      $best = $v
      $bestIdx = $i
    }
  }
}

$evalIdx = -1
for ($j = $bestIdx; $j -ge 0; $j--) {
  if ($lines[$j] -match '^Evaluating parameters:\s*VectorD\(') {
    $evalIdx = $j
    break
  }
}

Write-Output ("BEST_FITNESS=$best")
Write-Output ("bestFitnessLineIndex=$bestIdx")
Write-Output ("bestFitnessLine=$($lines[$bestIdx])")
Write-Output ("evalLineIndex=$evalIdx")
Write-Output ("evalLine=$($lines[$evalIdx])")

$start = [Math]::Max(0, $evalIdx - 8)
$end   = [Math]::Min($lines.Count - 1, $bestIdx + 8)
Write-Output '---CONTEXT---'
$lines[$start..$end]
