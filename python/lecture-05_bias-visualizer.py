import json
from itertools import product

import numpy as np
import plotly.graph_objects as go
from plotly.subplots import make_subplots

RNG_SEED = 42
N_TOTAL = 240          # training-set size (fixed across all compositions)
GROUP_RULE_COEF = 1.0  # how differently Group A vs B's true rule weighs eye
                       # openness -- bigger = more divergent rules = bigger gap
NOISE_STD = 0.10       # label noise; smaller = cleaner signal = bigger gap
PCT_STEPS = list(range(0, 101, 10))    # Group B share of training set
HAPPY_STEPS = list(range(0, 101, 10))  # target ("happy") balance within a group


# ---------------------------------------------------------------
# 1. Synthetic "emoji" data generators
# ---------------------------------------------------------------

def _score(x1, x2, group):
    return x1 + GROUP_RULE_COEF * x2 if group == 'A' else x1 - GROUP_RULE_COEF * x2


def make_group_data_natural(n, group, rng):
    """
    Each sample has 2 features: x1 = mouth curvature, x2 = eye openness.
    Natural (unforced) label rule -- used only for the fixed test set, so
    per-group accuracy is always measured against an unmanipulated ground
    truth no matter what the training sliders are set to.
    """
    x1 = rng.uniform(-1, 1, n)
    x2 = rng.uniform(-1, 1, n)
    noise = rng.normal(0, NOISE_STD, n)
    y = (_score(x1, x2, group) + noise > 0).astype(int)
    return np.stack([x1, x2], axis=1), y


def make_group_data_biased(n, group, rng, happy_pct):
    """
    Same feature generator as above, but the label is thresholded so that
    exactly `happy_pct` percent of this group's samples come out "happy".
    This lets a slider directly control each group's target balance,
    independent of group size and independent of the other group.
    """
    if n == 0:
        return np.zeros((0, 2)), np.zeros((0,), dtype=int)
    x1 = rng.uniform(-1, 1, n)
    x2 = rng.uniform(-1, 1, n)
    noise = rng.normal(0, NOISE_STD, n)
    score = _score(x1, x2, group) + noise
    if happy_pct >= 100:
        y = np.ones(n, dtype=int)
    elif happy_pct <= 0:
        y = np.zeros(n, dtype=int)
    else:
        thresh = np.quantile(score, 1 - happy_pct / 100)
        y = (score > thresh).astype(int)
    return np.stack([x1, x2], axis=1), y


def build_training_set(pct_b, happy_a, happy_b, rng):
    n_b = int(round(N_TOTAL * pct_b / 100))
    n_a = N_TOTAL - n_b
    Xa, ya = make_group_data_biased(n_a, 'A', rng, happy_a)
    Xb, yb = make_group_data_biased(n_b, 'B', rng, happy_b)
    X = np.concatenate([Xa, Xb])
    y = np.concatenate([ya, yb])
    g = np.array(['A'] * n_a + ['B'] * n_b)
    return X, y, g


# Fixed, BALANCED test set (always 50/50 A vs B, natural labels) so
# per-group accuracy is always a fair comparison, regardless of slider values.
_test_rng = np.random.default_rng(123)
_Xa_t, _ya_t = make_group_data_natural(150, 'A', _test_rng)
_Xb_t, _yb_t = make_group_data_natural(150, 'B', _test_rng)
X_TEST = np.concatenate([_Xa_t, _Xb_t])
Y_TEST = np.concatenate([_ya_t, _yb_t])
G_TEST = np.array(['A'] * 150 + ['B'] * 150)


# ---------------------------------------------------------------
# 2. Tiny neural network (plain numpy, one hidden layer)
# ---------------------------------------------------------------

class TinyNet:
    def __init__(self, n_in=2, n_hidden=8, rng=None):
        rng = rng or np.random.default_rng(0)
        self.W1 = rng.normal(0, 0.5, (n_in, n_hidden))
        self.b1 = np.zeros(n_hidden)
        self.W2 = rng.normal(0, 0.5, (n_hidden, 1))
        self.b2 = np.zeros(1)

    def forward(self, X):
        a1 = np.tanh(X @ self.W1 + self.b1)
        z2 = a1 @ self.W2 + self.b2
        return 1 / (1 + np.exp(-z2)).ravel()

    def train(self, X, y, epochs=100, lr=0.8):
        y = y.reshape(-1, 1).astype(float)
        n = len(X)
        for _ in range(epochs):
            a1 = np.tanh(X @ self.W1 + self.b1)
            a2 = 1 / (1 + np.exp(-(a1 @ self.W2 + self.b2)))

            dz2 = (a2 - y) / n
            dW2 = a1.T @ dz2
            db2 = dz2.sum(axis=0)

            da1 = dz2 @ self.W2.T
            dz1 = da1 * (1 - a1 ** 2)
            dW1 = X.T @ dz1
            db1 = dz1.sum(axis=0)

            self.W1 -= lr * dW1
            self.b1 -= lr * db1
            self.W2 -= lr * dW2
            self.b2 -= lr * db2

    def predict(self, X):
        return (self.forward(X) > 0.5).astype(int)


def accuracy(y_true, y_pred):
    return (y_true == y_pred).mean()


# ---------------------------------------------------------------
# 3. Precompute per-group accuracy + composition for every
#    (group-size, Group A balance, Group B balance) combination
# ---------------------------------------------------------------

def compute_frame_data(pct_b, happy_a, happy_b):
    rng = np.random.default_rng(RNG_SEED + pct_b * 10_000 + happy_a * 100 + happy_b)
    X_train, y_train, g_train = build_training_set(pct_b, happy_a, happy_b, rng)

    net = TinyNet(rng=np.random.default_rng(7))
    net.train(X_train, y_train)

    y_pred = net.predict(X_TEST)
    mask_a, mask_b = G_TEST == 'A', G_TEST == 'B'
    acc_overall = accuracy(Y_TEST, y_pred)
    acc_a = accuracy(Y_TEST[mask_a], y_pred[mask_a])
    acc_b = accuracy(Y_TEST[mask_b], y_pred[mask_b])

    a_happy = int(((g_train == 'A') & (y_train == 1)).sum())
    a_sad = int(((g_train == 'A') & (y_train == 0)).sum())
    b_happy = int(((g_train == 'B') & (y_train == 1)).sum())
    b_sad = int(((g_train == 'B') & (y_train == 0)).sum())

    return acc_overall, acc_a, acc_b, (a_happy, a_sad, b_happy, b_sad)


frame_cache = {
    (pct_b, happy_a, happy_b): compute_frame_data(pct_b, happy_a, happy_b)
    for pct_b, happy_a, happy_b in product(PCT_STEPS, HAPPY_STEPS, HAPPY_STEPS)
}


# ---------------------------------------------------------------
# 4. Build the figure: composition chart + accuracy chart, side by side
# ---------------------------------------------------------------

fig = make_subplots(
    rows=1, cols=2,
    column_widths=[0.42, 0.58],
    subplot_titles=("Training set composition", "Accuracy on balanced test set"),
    horizontal_spacing=0.12,
)

INIT_PCT, INIT_HAPPY_A, INIT_HAPPY_B = 10, 50, 50
acc_o0, acc_a0, acc_b0, counts0 = frame_cache[(INIT_PCT, INIT_HAPPY_A, INIT_HAPPY_B)]
a_happy0, a_sad0, b_happy0, b_sad0 = counts0

fig.add_trace(
    go.Bar(
        x=['Light faces', 'Dark faces'], y=[a_happy0, b_happy0],
        name='Happy', marker_color='#1b9e77',
        legendgroup='label',
    ),
    row=1, col=1,
)
fig.add_trace(
    go.Bar(
        x=['Light faces', 'Dark faces'], y=[a_sad0, b_sad0],
        name='Sad', marker_color='#d95f02',
        legendgroup='label',
    ),
    row=1, col=1,
)
fig.add_trace(
    go.Bar(
        x=['Overall', 'Light faces', 'Dark faces'],
        y=[acc_o0, acc_a0, acc_b0],
        marker_color=['#888780', '#1b9e77', '#7f77dd'],
        text=[f'{v:.0%}' for v in [acc_o0, acc_a0, acc_b0]],
        textposition='outside',
        showlegend=False,
        hoverinfo='none',
    ),
    row=1, col=2,
)

fig.update_yaxes(title_text='training samples', range=[0, N_TOTAL * 1.05], row=1, col=1)
fig.update_yaxes(title_text='accuracy', range=[0, 1.15], tickformat='.0%', row=1, col=2)

fig.update_layout(
    barmode='stack',
    height=440,
    margin=dict(t=70, b=40),
    legend=dict(orientation='h', y=1, x=0.25, title_text='Label'),
    legend_title_side='top',
)

# ---------------------------------------------------------------
# 5. Frames -- one per (pct_b, happy_a, happy_b) combination
# ---------------------------------------------------------------

frames = []
for pct_b, happy_a, happy_b in product(PCT_STEPS, HAPPY_STEPS, HAPPY_STEPS):
    acc_o, acc_a, acc_b, counts = frame_cache[(pct_b, happy_a, happy_b)]
    a_happy, a_sad, b_happy, b_sad = counts
    frames.append(go.Frame(
        name=f"{pct_b}_{happy_a}_{happy_b}",
        data=[
            go.Bar(y=[a_happy, b_happy]),
            go.Bar(y=[a_sad, b_sad]),
            go.Bar(y=[acc_o, acc_a, acc_b],
                   text=[f'{v:.0%}' for v in [acc_o, acc_a, acc_b]]),
        ],
    ))
fig.frames = frames


# ---------------------------------------------------------------
# 6. Custom sliders
#
# Plotly's built-in slider widget only supports one dimension cleanly
# driving frame selection. With three independent sliders jointly
# selecting a precomputed frame, we use plain <input type="range">
# elements instead, styled to match, and wire them up with a little JS
# that composes the right frame name and calls Plotly.animate directly.
# ---------------------------------------------------------------

def _slider_block(slider_id, label, steps, init_value, accent):
    init_index = steps.index(init_value)
    return f'''
    <div class="slider-block">
      <div class="slider-label">
        <span>{label}</span>
        <span id="{slider_id}-value" class="slider-value">{init_value}%</span>
      </div>
      <input type="range" id="{slider_id}" class="nice-slider"
             min="0" max="{len(steps) - 1}" step="1" value="{init_index}"
             style="--accent: {accent}">
    </div>'''


def build_html():
    plot_html = fig.to_html(
        include_plotlyjs='cdn',
        full_html=False,
        div_id='biasChart',
        config={'displayModeBar': False},
    )

    sliders_html = (
        _slider_block('pctSlider', 'Percentage of dark faces in training set',
                      PCT_STEPS, INIT_PCT, '#7f77dd')
        + _slider_block('happyASlider', 'Label balance for light faces (% happy)',
                        HAPPY_STEPS, INIT_HAPPY_A, '#1b9e77')
        + _slider_block('happyBSlider', 'Label balance for dark faces (% happy)',
                        HAPPY_STEPS, INIT_HAPPY_B, '#d95f02')
    )

    return f'''<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<title>Training data composition and model bias</title>
<style>
  body {{
    font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, Helvetica, Arial, sans-serif;
    color: #0b0b0b;
    max-width: 860px;
    margin: 0 auto;
    padding: 1.5rem;
  }}
  .sliders {{
    display: flex;
    flex-direction: column;
    gap: 1.15rem;
    margin-top: 1rem;
    padding: 1.1rem 1.35rem;
    background: #f6f5f0;
    border-radius: 10px;
  }}
  .slider-label {{
    display: flex;
    justify-content: space-between;
    font-size: 13px;
    color: #52514e;
    margin-bottom: 7px;
  }}
  .slider-value {{
    font-weight: 600;
    color: #0b0b0b;
    min-width: 2.5em;
    text-align: right;
  }}
  input.nice-slider {{
    -webkit-appearance: none;
    appearance: none;
    width: 100%;
    height: 4px;
    border-radius: 2px;
    background: #ddd9cd;
    outline: none;
  }}
  input.nice-slider::-webkit-slider-runnable-track {{
    height: 4px;
    border-radius: 2px;
    background: #ddd9cd;
  }}
  input.nice-slider::-webkit-slider-thumb {{
    -webkit-appearance: none;
    appearance: none;
    width: 16px;
    height: 16px;
    border-radius: 50%;
    background: var(--accent);
    border: 2px solid white;
    box-shadow: 0 0 0 1px rgba(0,0,0,0.08);
    cursor: pointer;
    margin-top: -6px;
  }}
  input.nice-slider::-moz-range-track {{
    height: 4px;
    border-radius: 2px;
    background: #ddd9cd;
  }}
  input.nice-slider::-moz-range-thumb {{
    width: 16px;
    height: 16px;
    border-radius: 50%;
    background: var(--accent);
    border: 2px solid white;
    box-shadow: 0 0 0 1px rgba(0,0,0,0.08);
    cursor: pointer;
  }}
</style>
</head>
<body>

{plot_html}

<div class="sliders">
  {sliders_html}
</div>

<script>
(function() {{
  const PCT_STEPS = {json.dumps(PCT_STEPS)};
  const HAPPY_STEPS = {json.dumps(HAPPY_STEPS)};

  const pctSlider = document.getElementById('pctSlider');
  const haSlider = document.getElementById('happyASlider');
  const hbSlider = document.getElementById('happyBSlider');
  const gd = document.getElementById('biasChart');

  function currentValues() {{
    return [
      PCT_STEPS[parseInt(pctSlider.value, 10)],
      HAPPY_STEPS[parseInt(haSlider.value, 10)],
      HAPPY_STEPS[parseInt(hbSlider.value, 10)],
    ];
  }}

  function updateLabels(pct, ha, hb) {{
    document.getElementById('pctSlider-value').textContent = pct + '%';
    document.getElementById('happyASlider-value').textContent = ha + '%';
    document.getElementById('happyBSlider-value').textContent = hb + '%';
  }}

  function animateToCurrent() {{
    const [pct, ha, hb] = currentValues();
    updateLabels(pct, ha, hb);
    const frameName = pct + '_' + ha + '_' + hb;
    Plotly.animate(gd, [frameName], {{
      frame: {{ duration: 0, redraw: true }},
      transition: {{ duration: 0 }},
      mode: 'immediate',
    }});
  }}

  [pctSlider, haSlider, hbSlider].forEach(function(el) {{
    el.addEventListener('input', animateToCurrent);
  }});
  updateLabels({INIT_PCT}, {INIT_HAPPY_A}, {INIT_HAPPY_B});
}})();
</script>

</body>
</html>'''


if __name__ == '__main__':
    with open('bias_visualizer.html', 'w') as f:
        f.write(build_html())