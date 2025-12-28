import { CommonModule, DatePipe } from '@angular/common';
import { Component, inject, OnDestroy, OnInit } from '@angular/core';
import { FormsModule } from '@angular/forms';
import { Subscription } from 'rxjs';
import Swal from 'sweetalert2';
import { BlockedUserResponse } from '../../../responses/banbe/blocked_user_response';
import { FriendRequestItemResponse } from '../../../responses/banbe/friend_request_item_response';
import { FriendSuggestionResponse } from '../../../responses/banbe/friend_suggestion_response';
import { FriendSummaryResponse } from '../../../responses/banbe/friend_summary_response';
import { FriendEventService } from '../../../services/friend-event.service';
import { Base } from '../../base/base';

@Component({
  selector: 'app-user-friend',
  imports: [FormsModule, DatePipe, CommonModule],
  templateUrl: './user-friend.html',
  styleUrl: './user-friend.scss',
  standalone: true,
})
export class UserFriend extends Base implements OnInit, OnDestroy {
  private friendEventService = inject(FriendEventService);
  private eventSubscription?: Subscription;

  // Tab hiện tại: 'friends' | 'incoming' | 'outgoing' | 'suggestions' | 'blocked' | 'search'
  active_tab: 'friends' | 'incoming' | 'outgoing' | 'suggestions' | 'blocked' | 'search' =
    'friends';

  // Loading flags
  loading_friends = false;
  loading_incoming = false;
  loading_outgoing = false;
  loading_suggestions = false;
  loading_blocked = false;
  loading_search = false;

  // Dữ liệu
  friends: FriendSummaryResponse[] = [];
  incoming_requests: FriendRequestItemResponse[] = [];
  outgoing_requests: FriendRequestItemResponse[] = [];
  suggestions: FriendSuggestionResponse[] = [];
  blocked_users: BlockedUserResponse[] = [];
  search_results: FriendSummaryResponse[] = [];

  // Gửi lời mời nhanh theo user_id (tạm thời, sau này có search theo username/profile thì nâng cấp)
  new_friend_id: number | null = null;
  sending_request = false;

  // Search
  search_keyword = '';
  search_sending = new Set<number>(); // Track user_ids đang gửi lời mời

  ngOnInit(): void {
    // Mặc định load tab bạn bè
    this.loadFriends();
    // Load suggestions ngay để hiển thị ở sidebar hoặc khi vào tab
    this.loadSuggestions();

    // Subscribe to friend events from WebSocket notifications
    this.eventSubscription = this.friendEventService.events$.subscribe((event) => {
      switch (event.type) {
        case 'FRIEND_REQUEST_RECEIVED':
          // Có lời mời mới -> reload incoming
          if (this.active_tab === 'incoming') {
            this.loadIncoming();
          } else {
            // Reset để buộc reload khi chuyển tab
            this.incoming_requests = [];
          }
          break;
        case 'FRIEND_REQUEST_ACCEPTED':
          // Lời mời được chấp nhận -> reload friends và outgoing
          this.loadFriends();
          if (this.active_tab === 'outgoing') {
            this.loadOutgoing();
          } else {
            this.outgoing_requests = [];
          }
          break;
        case 'FRIEND_REQUEST_DECLINED':
          // Lời mời bị từ chối -> reload outgoing
          if (this.active_tab === 'outgoing') {
            this.loadOutgoing();
          } else {
            this.outgoing_requests = [];
          }
          break;
      }
    });
  }

  ngOnDestroy(): void {
    this.eventSubscription?.unsubscribe();
  }

  // ----- TAB -----
  setTab(tab: 'friends' | 'incoming' | 'outgoing' | 'suggestions' | 'blocked' | 'search'): void {
    this.active_tab = tab;

    if (tab === 'friends' && this.friends.length === 0) {
      this.loadFriends();
    } else if (tab === 'incoming') {
      // Luôn reload incoming để đảm bảo dữ liệu mới nhất
      this.loadIncoming();
    } else if (tab === 'outgoing') {
      // Luôn reload outgoing để đảm bảo dữ liệu mới nhất (fix bug từ chối còn lưu)
      this.loadOutgoing();
    } else if (tab === 'suggestions' && this.suggestions.length === 0) {
      this.loadSuggestions();
    } else if (tab === 'blocked') {
      this.loadBlockedUsers();
    }
  }

  // ----- LOAD DATA -----
  loadFriends(): void {
    this.loading_friends = true;
    this.friendService.getFriends().subscribe({
      next: (res) => {
        this.friends = res.data || [];
        this.loading_friends = false;
      },
      error: () => {
        this.loading_friends = false;
      },
    });
  }

  loadIncoming(): void {
    this.loading_incoming = true;
    this.friendService.getIncomingRequests().subscribe({
      next: (res) => {
        this.incoming_requests = res.data || [];
        this.loading_incoming = false;
      },
      error: () => {
        this.loading_incoming = false;
      },
    });
  }

  loadOutgoing(): void {
    this.loading_outgoing = true;
    this.friendService.getOutgoingRequests().subscribe({
      next: (res) => {
        this.outgoing_requests = res.data || [];
        this.loading_outgoing = false;
      },
      error: () => {
        this.loading_outgoing = false;
      },
    });
  }

  loadSuggestions(): void {
    this.loading_suggestions = true;
    this.friendService.getSuggestions(10).subscribe({
      next: (res) => {
        this.suggestions = res.data || [];
        this.loading_suggestions = false;
      },
      error: () => {
        this.loading_suggestions = false;
      },
    });
  }

  loadBlockedUsers(): void {
    this.loading_blocked = true;
    this.friendService.getBlockedUsers().subscribe({
      next: (res) => {
        this.blocked_users = res.data || [];
        this.loading_blocked = false;
      },
      error: () => {
        this.loading_blocked = false;
      },
    });
  }

  searchUsers(): void {
    if (!this.search_keyword || this.search_keyword.trim().length < 2) {
      Swal.fire('Thông báo', 'Vui lòng nhập ít nhất 2 ký tự để tìm kiếm', 'info');
      return;
    }

    this.loading_search = true;
    this.friendService.searchUsers(this.search_keyword.trim(), 20).subscribe({
      next: (res) => {
        this.search_results = res.data || [];
        this.loading_search = false;
      },
      error: () => {
        this.loading_search = false;
      },
    });
  }

  // ----- ACTIONS -----

  /** Gửi lời mời kết bạn nhanh bằng user_id */
  sendFriendRequest(): void {
    if (!this.new_friend_id || this.new_friend_id <= 0) {
      Swal.fire('Thông báo', 'Vui lòng nhập user ID hợp lệ', 'info');
      return;
    }

    this.sending_request = true;
    this.friendService.sendRequest({ target_user_id: this.new_friend_id }).subscribe({
      next: (res) => {
        this.sending_request = false;
        Swal.fire('Thành công', 'Đã gửi lời mời kết bạn', 'success');
        this.new_friend_id = null;
        // reload danh sách đã gửi (nếu đang ở tab đó)
        if (this.active_tab === 'outgoing') {
          this.loadOutgoing();
        }
      },
      error: (err) => {
        this.sending_request = false;
        console.error('sendFriendRequest error', err);
        const msg = err?.error?.message || 'Gửi lời mời kết bạn thất bại';
        Swal.fire('Lỗi', msg, 'error');
      },
    });
  }

  /** Chấp nhận lời mời đến */
  acceptRequest(req: FriendRequestItemResponse): void {
    this.friendService.acceptRequest(req.request_id).subscribe({
      next: () => {
        Swal.fire('Thành công', 'Đã chấp nhận lời mời kết bạn', 'success');
        this.loadIncoming();
        this.loadFriends();
      },
      error: (err) => {
        console.error('acceptRequest error', err);
        const msg = err?.error?.message || 'Không thể chấp nhận lời mời';
        Swal.fire('Lỗi', msg, 'error');
      },
    });
  }

  /** Từ chối lời mời đến */
  declineRequest(req: FriendRequestItemResponse): void {
    this.friendService.declineRequest(req.request_id).subscribe({
      next: () => {
        Swal.fire('Thành công', 'Đã từ chối lời mời kết bạn', 'success');
        this.loadIncoming();
      },
      error: (err) => {
        console.error('declineRequest error', err);
        const msg = err?.error?.message || 'Không thể từ chối lời mời';
        Swal.fire('Lỗi', msg, 'error');
      },
    });
  }

  /** Huỷ lời mời đã gửi */
  cancelRequest(req: FriendRequestItemResponse): void {
    this.friendService.cancelRequest(req.request_id).subscribe({
      next: () => {
        Swal.fire('Thành công', 'Đã huỷ lời mời kết bạn', 'success');
        this.loadOutgoing();
      },
      error: (err) => {
        console.error('cancelRequest error', err);
        const msg = err?.error?.message || 'Không thể huỷ lời mời';
        Swal.fire('Lỗi', msg, 'error');
      },
    });
  }

  /** Huỷ kết bạn */
  unfriend(friend: FriendSummaryResponse): void {
    Swal.fire({
      title: 'Xác nhận',
      text: `Bạn có chắc muốn huỷ kết bạn với ${friend.ho_ten}?`,
      icon: 'warning',
      showCancelButton: true,
      confirmButtonText: 'Huỷ kết bạn',
      cancelButtonText: 'Đóng',
    }).then((result) => {
      if (!result.isConfirmed) return;

      this.friendService.unfriend(friend.user_id).subscribe({
        next: () => {
          Swal.fire('Thành công', 'Đã huỷ kết bạn', 'success');
          this.loadFriends();
        },
        error: (err) => {
          console.error('unfriend error', err);
          const msg = err?.error?.message || 'Không thể huỷ kết bạn';
          Swal.fire('Lỗi', msg, 'error');
        },
      });
    });
  }

  getAvatar(avatarUrl: string | null | undefined, name: string): string {
    if (avatarUrl) {
      return `http://localhost:8088/api/v1/users/profile-images/${avatarUrl}`;
    }
    return `https://ui-avatars.com/api/?name=${name}&background=random&color=fff&size=128`;
  }

  navigateChatWithFriend(f: FriendSummaryResponse) {
    this.router.navigate(['/chat', f.user_id]).then();
  }

  // ============== SUGGESTIONS ==============

  /** Gửi lời mời từ gợi ý */
  sendRequestFromSuggestion(user: FriendSuggestionResponse): void {
    this.search_sending.add(user.user_id);
    this.friendService.sendRequest({ target_user_id: user.user_id }).subscribe({
      next: () => {
        this.search_sending.delete(user.user_id);
        Swal.fire({
          toast: true,
          position: 'top-end',
          icon: 'success',
          title: 'Đã gửi lời mời kết bạn!',
          timer: 2000,
          showConfirmButton: false,
        });
        // Xóa khỏi danh sách suggestions
        this.suggestions = this.suggestions.filter((s) => s.user_id !== user.user_id);
      },
      error: (err) => {
        this.search_sending.delete(user.user_id);
        const msg = err?.error?.message || 'Không thể gửi lời mời';
        Swal.fire('Lỗi', msg, 'error');
      },
    });
  }

  /** Gửi lời mời từ kết quả tìm kiếm */
  sendRequestFromSearch(user: FriendSummaryResponse): void {
    this.search_sending.add(user.user_id);
    this.friendService.sendRequest({ target_user_id: user.user_id }).subscribe({
      next: () => {
        this.search_sending.delete(user.user_id);
        Swal.fire({
          toast: true,
          position: 'top-end',
          icon: 'success',
          title: 'Đã gửi lời mời kết bạn!',
          timer: 2000,
          showConfirmButton: false,
        });
        // Xóa khỏi danh sách search
        this.search_results = this.search_results.filter((s) => s.user_id !== user.user_id);
      },
      error: (err) => {
        this.search_sending.delete(user.user_id);
        const msg = err?.error?.message || 'Không thể gửi lời mời';
        Swal.fire('Lỗi', msg, 'error');
      },
    });
  }

  isSendingRequest(userId: number): boolean {
    return this.search_sending.has(userId);
  }

  // ============== BLOCK ==============

  /** Chặn một người dùng */
  blockUser(user: FriendSummaryResponse): void {
    Swal.fire({
      title: 'Chặn người dùng?',
      text: `Bạn có chắc muốn chặn ${user.ho_ten}? Người này sẽ không thể gửi tin nhắn hay kết bạn với bạn.`,
      icon: 'warning',
      showCancelButton: true,
      confirmButtonText: 'Chặn',
      cancelButtonText: 'Hủy',
      confirmButtonColor: '#ef4444',
    }).then((result) => {
      if (!result.isConfirmed) return;

      this.friendService.blockUser({ target_user_id: user.user_id }).subscribe({
        next: () => {
          Swal.fire('Đã chặn', `${user.ho_ten} đã bị chặn`, 'success');
          this.loadFriends();
          this.loadBlockedUsers();
        },
        error: (err) => {
          const msg = err?.error?.message || 'Không thể chặn người dùng';
          Swal.fire('Lỗi', msg, 'error');
        },
      });
    });
  }

  /** Bỏ chặn một người dùng */
  unblockUser(blocked: BlockedUserResponse): void {
    Swal.fire({
      title: 'Bỏ chặn?',
      text: `Bạn có chắc muốn bỏ chặn ${blocked.ho_ten}?`,
      icon: 'question',
      showCancelButton: true,
      confirmButtonText: 'Bỏ chặn',
      cancelButtonText: 'Hủy',
    }).then((result) => {
      if (!result.isConfirmed) return;

      this.friendService.unblockUser(blocked.user_id).subscribe({
        next: () => {
          Swal.fire('Đã bỏ chặn', `${blocked.ho_ten} đã được bỏ chặn`, 'success');
          this.loadBlockedUsers();
        },
        error: (err) => {
          const msg = err?.error?.message || 'Không thể bỏ chặn';
          Swal.fire('Lỗi', msg, 'error');
        },
      });
    });
  }

  // ============== HELPERS ==============

  getReasonLabel(reason: string): string {
    switch (reason) {
      case 'SAME_BATTLE':
        return 'Đã chơi cùng';
      case 'MUTUAL_FRIEND':
        return 'Bạn chung';
      case 'POPULAR':
        return 'Phổ biến';
      default:
        return '';
    }
  }

  getReasonIcon(reason: string): string {
    switch (reason) {
      case 'SAME_BATTLE':
        return 'fa-gamepad';
      case 'MUTUAL_FRIEND':
        return 'fa-users';
      case 'POPULAR':
        return 'fa-star';
      default:
        return 'fa-user';
    }
  }
}
