
### Differential equations
\begin{align*}
\frac{dx(c,t)}{dt} &= ( k_s(c,t) - k_d(c,t) ) \cdot x(t) \\
\frac{dd(c,t)}{dt} &= k_d(c,t) \cdot x(t)
\end{align*}

### Definitions
\begin{align*}
x(c,t) &= \text{the amount of \textbf{\textit{viable cells}} at time \textit{t} at drug concentration \textit{c}} \\
d(c,t) &= \text{the amount of \textbf{\textit{dead cells}} at time \textit{t} at drug concentration \textit{c}} \\
x_0 &= x(0,0) = \text{starting amount of \textbf{\textit{viable cells}}} \\
d_0 &= d(0,0) = \text{starting amount of \textbf{\textit{dead cells}}}
\end{align*}

### Dratio (and ctrl) ratio of increase in dead to increase in living
\begin{align*}
D_{ratio}(c) &= \frac{ \max{\{ 1, d(c,t) - d_0 \}} }{ x(c,t) - x_0 } = \frac{\text{increase in dead cells (treated)}}{\text{increase in living cells (treated)}} \\
D_{ratio\_ctrl} &= \frac{ \max{\{ 1, d(0,t) - d_0 \}} }{ x(0,t) - x_0 } = \frac{\text{increase in dead cells (un-treated)}}{\text{increase in living cells (un-treated)}}
\end{align*}

### k(c) and k(0) (number of cell divisions in treated and control)
Growth-rate (treated and control/un-treated), i.e. number of cell divisions during assay:
\begin{align*}
k(c) &= \frac{1}{t} \log_2{\left( \frac{x(c,t)}{x_0 } \right) } \quad \text{(treated)} \\
k(0) &= \frac{1}{t} \log_2{\left( \frac{x(0,t)}{x_0 } \right) } \quad \text{(un-treated)}
\end{align*}

### GR value
$$
GR(c) = 2^\frac{k(c)}{k(0)} - 1
$$

### k_s and k_d

k_s(c) &= k(c) \cdot (1 + D_{ratio}) \\
k_d(c) &= k(c) \cdot D_{ratio}


### GR_static and GR_toxic
#### with k_s and k_d
$$
GR_{static} = 2 \string^ \left\{{\frac{ k_s(c) }{ k_s(0) } } \right\} - 1
$$

$$
GR_{toxic} = 2 \string^ \left\{ {\frac{ k_d(0) - k_d(c) }{ hrs/24 } } \right\} - 1
$$

#### with gr and Dratio
\begin{align*}
GR_{static} &= 2 \string^ \left\{{\frac{ gr_{trt}(c) \cdot (1 + D_{ratio}) }{ gr_{ctrl} \cdot (1 + D_{ratio\_ctrl}) } } \right\} - 1 \\
GR_{toxic} &= 2 \string^ \left\{ {\frac{ gr_{ctrl} \cdot D_{ratio\_ctrl} - gr_{trt}(c) \cdot D_{ratio} }{ hrs/24 } } \right\} - 1
\end{align*}



#### May 22 (lots of equations for GR_static and GR_toxic)

\begin{align*}
k(c) &= \frac{1}{t} \log_2{\left( \frac{x(c,t)}{x_0 } \right) } \quad \text{(treated)} \\
k(0) &= \frac{1}{t} \log_2{\left( \frac{x(0,t)}{x_0 } \right) } \quad \text{(un-treated)} \\
\bold{D_{ratio}(c)} &= \bold{\frac{ \max{\{ 1, d(c,t) - d_0 \}} }{ x(c,t) - x_0 } = \frac{\text{increase in dead cells}}{\text{increase in living cells}}} \\
\bold{k_s(c)} &= \bold{k(c) \cdot (1 + D_{ratio}(c))} \\
\bold{k_d(c)} &= \bold{k(c) \cdot D_{ratio}(c) }\\
\bold{gr_s(c)} &= \bold{ \frac{k_s(c)}{k_s(0)} } \\
\bold{gr_d(c)} &= \bold{ k_d(0) - k_d(c) } \\
\bold{GR_{static}(c)} &= \bold{2^{gr_s(c)} - 1 }\\
\bold{GR_{toxic}(c)} &= \bold{2^{gr_d(c) } - 1 } \\
\bold{GR_{static}(c)} &= \bold{2^\frac{ k_s(c) }{ k_s(0) } - 1 }\\
\bold{GR_{toxic}(c)} &= \bold{2^{ k_d(0) - k_d(c) }- 1 } \\
\bold{GR_{static}(c)} &= \bold{2 \string^ \left\{{\frac{ k(c) \cdot (1 + D_{ratio}(c)) }{ k(0) \cdot (1 + D_{ratio}(0)) } } \right\} - 1 }\\
\bold{GR_{toxic}(c)} &= \bold{2^{k(0) \cdot D_{ratio}(0) - k(c) \cdot D_{ratio}(c) } - 1 }
\end{align*}


#### GR_static and toxic as functions of k_s and k_d
\begin{align*}
\bold{GR_{static}(c)} &= \bold{2^{k_s(c)/k_s(0) } - 1 }\\
\bold{GR_{toxic}(c)} &= \bold{2^{ k_d(0) - k_d(c) }- 1 }
\end{align*}


#### dead/live change ratio and total total change ratio
\bold{k(c)} &= \bold{\frac{1}{t} log_2{\left( \frac{x(c,t)}{x_0 } \right) }} \\
\bold{r(c)} &= \bold{\frac{d(c,t) - d_0}{ x(c,t) - x_0 } = \frac{\text{change in dead cell count}}{\text{change in live cell count}}} \\

#### k_s and k_d
\begin{align*}
\bold{k_s(c)} &= \bold{k(c) \cdot (1 + r(c))} \\
\bold{k_d(c)} &= \bold{k(c) \cdot r(c) }
\end{align*}