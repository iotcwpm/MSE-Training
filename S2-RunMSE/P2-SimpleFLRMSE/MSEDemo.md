% MSE Demonstration
% T. KITAKADO, I. MOSQUEIRA, R. SHARMA (WPM)
% SC16 December 2013

# SIMULATION STEPS by year

1. Get catch y-1
2. CPUE = SSB + E()
3. lm (cpue ~ year) for last 5 years
4. C_y = C_y+1 * (1 + beta * slope)
5. Project stock w/ C_y + SR

Done for each iteration, HCR and OM option

# SCENARIOS

- OMs: Simulated populations w/ tuna LH
	- ow: One-way trip, F up to 0.80*Fcrash
	- ed: Effort dynamics, F to drive stock to MSY
	- rc: Roller coaster, F up and then down
- Recruitment residuals: 10, 20 or 40 years
- Beta parameter in HCR: 0.1, 0.2, 0.4
- Noise in CPUE signal: 0, 10, 20 SD in Norm()

		> 3*3*3*3 = 81

# 

\centering\includegraphics[width=0.80\textwidth]{graphics/om.png}

# 

 \centering \Large http://spark.rstudio.com/imosqueira/IOTCMSE/
