const viewers = {};

function initViewer(containerId) {
  const viewer = $3Dmol.createViewer(containerId, { backgroundColor: 'white' });
  viewer.render();

  viewers[containerId] = {
    viewer,
    protName: 'screenshot',
    isSpinning: true
  };

  const spinCheckbox = document.querySelector(`[data-spin="${containerId}"]`);
  if (spinCheckbox) {
    spinCheckbox.addEventListener('change', (event) => {
      const spin = event.target.checked;
      viewers[containerId].isSpinning = spin;
      viewer.spin(spin);
    });
  }

  const screenshotButton = document.querySelector(`[data-screenshot="${containerId}"]`);
  if (screenshotButton) {
    screenshotButton.addEventListener('click', () => {
      const canvas = viewer.getCanvas();
      if (!canvas) return;

      canvas.toBlob((blob) => {
        const a = document.createElement('a');
        a.href = URL.createObjectURL(blob);
        a.download = `${viewers[containerId].protName}.png`;
        a.click();
      }, 'image/png');
    });
  }
}

document.addEventListener('DOMContentLoaded', () => {
  document.querySelectorAll('[structure-viewer]').forEach(el => {
    initViewer(el.id);
  });
});

Shiny.addCustomMessageHandler('renderStructure', function(message) {
  const containerId = message.containerId;
  if (!(containerId in viewers)) return;

  const { viewer } = viewers[containerId];
  viewers[containerId].protName = message.protName || 'screenshot';

  viewer.clear();
  viewer.addModel(message.data);

  const colorMap = message.colorMap || {};
  viewer.setStyle({}, {
    cartoon: {
      colorfunc: atom => colorMap[atom.resi?.toString()] || 'white'
    }
  });

  viewer.zoomTo();
  viewer.render();
  viewer.spin(viewers[containerId].isSpinning);
});
