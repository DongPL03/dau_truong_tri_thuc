/**
 * Response gợi ý kết bạn
 */
export interface FriendSuggestionResponse {
  user_id: number;
  ho_ten: string;
  avatar_url: string | null;
  level: number;
  tong_diem: number;
  /**
   * Lý do gợi ý: "SAME_BATTLE", "MUTUAL_FRIEND", "POPULAR"
   */
  reason: 'SAME_BATTLE' | 'MUTUAL_FRIEND' | 'POPULAR';
  /**
   * Số bạn chung hoặc số trận đấu chung
   */
  mutual_friends_count: number;
}
