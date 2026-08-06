package flat

import flat.util.MetricCollector

final case class Config(inputs: Seq[os.Path] = Seq.empty, noError: Boolean = false, smtTimeLimit: Int = 3000,
                        metrics: Option[MetricCollector] = None, extractMode: Boolean = false)

final case class ExtractConfig(input: os.Path = null, output: os.Path = null, multiGoals: Boolean = false)
