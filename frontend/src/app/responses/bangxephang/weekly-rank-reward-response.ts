export interface WeeklyRankRewardResponse {
  gold_reward: number;
  rank_tier: 'BRONZE' | 'SILVER' | 'GOLD' | 'PLATINUM' | 'DIAMOND' | 'MASTER';
  global_rank: number | null;
  week_id: string;      // ví dụ "2025-51"
  claimed_before: boolean;
  gold_before: number;
  gold_after: number;
}
