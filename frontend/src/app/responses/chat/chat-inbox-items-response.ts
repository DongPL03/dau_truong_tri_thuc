export interface ChatInboxItemResponse {
  partner_id: number;
  partner_name: string;
  partner_avatar_url: string | null;
  last_message: string;
  last_time: string;   // ISO
  unread_count: number;
}
