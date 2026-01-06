export interface ChatInboxItemResponse {
  partnerId: number;
  partnerName: string;
  partnerAvatarUrl: string | null;
  lastMessage: string;
  lastTime: string; // ISO
  unreadCount: number;
}
