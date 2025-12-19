export interface MatchRewardResponse {
  xp_gained: number;
  gold_gained: number;
  level_before: number;
  level_after: number;
  rank_tier_before: 'BRONZE' | 'SILVER' | 'GOLD' | 'PLATINUM' | 'DIAMOND' | 'MASTER';
  rank_tier_after: 'BRONZE' | 'SILVER' | 'GOLD' | 'PLATINUM' | 'DIAMOND' | 'MASTER';
}
