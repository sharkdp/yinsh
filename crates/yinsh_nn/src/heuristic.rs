use runnt::activation::Activation;
use runnt::nn::NN;

use tracing::debug;
use yinsh::GameState;
use yinsh_ai::Heuristic;

use crate::features::{FEATURE_SIZE, extract_features};

/// Evaluation score type (matches minimax crate)
pub type Evaluation = i32;

/// Neural network-based heuristic for Yinsh.
pub struct NNHeuristic {
    network: NN,
    scale: Evaluation,
}

impl NNHeuristic {
    /// Create a new NNHeuristic from a trained network.
    ///
    /// The `scale` parameter controls the output range. The network outputs
    /// values in [-1, 1] (tanh activation), which are multiplied by `scale`
    /// to produce the final evaluation.
    pub fn new(network: NN, scale: Evaluation) -> Self {
        Self { network, scale }
    }

    /// Load a trained model from a file.
    pub fn load(path: &str, scale: Evaluation) -> Result<Self, runnt::error::Error> {
        let network = NN::load(path)?;
        Ok(Self::new(network, scale))
    }

    /// Create a new untrained network with the default architecture.
    ///
    /// Architecture: 172 → 64 (ReLU) → 32 (ReLU) → 1 (Tanh)
    pub fn new_untrained(scale: Evaluation) -> Self {
        let network = NN::new(&[FEATURE_SIZE, 64, 32, 1])
            .with_learning_rate(0.01)
            .with_activation_hidden(Activation::Relu)
            .with_activation_output(Activation::Tanh);

        Self::new(network, scale)
    }

    /// Get a mutable reference to the underlying network (for training).
    pub fn network_mut(&mut self) -> &mut NN {
        &mut self.network
    }

    /// Get a reference to the underlying network.
    pub fn network(&self) -> &NN {
        &self.network
    }

    /// Save the trained model to a file.
    pub fn save(&self, path: &str) {
        self.network.save(path)
    }
}

impl Heuristic for NNHeuristic {
    fn identifier(&self) -> String {
        format!("NNHeuristic {{ scale: {} }}", self.scale)
    }

    fn evaluate_for_player_a(&self, state: &GameState) -> minimax::Evaluation {
        let features = extract_features(state);
        let output = self.network.forward(&features);

        debug_assert!(
            output.len() == 1,
            "NNHeuristic network should have a single output neuron"
        );

        debug!("NN output score: {}", output[0]);

        // Network outputs in [-1, 1], scale to evaluation range
        (output[0] * self.scale as f32) as minimax::Evaluation
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_untrained_network_runs() {
        let heuristic = NNHeuristic::new_untrained(10_000);
        let state = GameState::initial();

        // Should not panic, output should be in valid range
        let eval = heuristic.evaluate_for_player_a(&state);
        assert!(eval >= -10_000 && eval <= 10_000);
    }
}
