import { CommonModule } from '@angular/common';
import { Component, OnInit, computed, inject, signal } from '@angular/core';
import { FormsModule } from '@angular/forms';
import Swal from 'sweetalert2';
import { MuaVatPhamResponse, ShopItem, ShopResponse } from '../../../models/vat-pham.model';
import { VatPhamService } from '../../../services/vat-pham.service';

@Component({
  selector: 'app-shop-vat-pham',
  standalone: true,
  imports: [CommonModule, FormsModule],
  templateUrl: './shop-vat-pham.html',
  styleUrl: './shop-vat-pham.scss',
})
export class ShopVatPham implements OnInit {
  private vatPhamService = inject(VatPhamService);

  // State
  shopItems = signal<ShopItem[]>([]);
  tienVang = signal<number>(0);
  loading = signal<boolean>(false);
  purchasing = signal<boolean>(false);

  // Filter & Sort
  filterRarity = signal<string>('ALL');
  sortBy = signal<string>('PRICE_ASC');

  // Computed: Filtered & Sorted items
  filteredItems = computed(() => {
    let items = [...this.shopItems()];

    // Filter by rarity
    const rarity = this.filterRarity();
    if (rarity !== 'ALL') {
      items = items.filter((item) => item.do_hiem === rarity);
    }

    // Sort
    const sort = this.sortBy();
    switch (sort) {
      case 'PRICE_ASC':
        items.sort((a, b) => a.gia_xu - b.gia_xu);
        break;
      case 'PRICE_DESC':
        items.sort((a, b) => b.gia_xu - a.gia_xu);
        break;
      case 'RARITY':
        const rarityOrder = { COMMON: 1, RARE: 2, EPIC: 3, LEGENDARY: 4 };
        items.sort((a, b) => (rarityOrder[b.do_hiem] || 0) - (rarityOrder[a.do_hiem] || 0));
        break;
    }

    return items;
  });

  ngOnInit(): void {
    this.loadShop();
  }

  loadShop(): void {
    this.loading.set(true);
    this.vatPhamService.getShop().subscribe({
      next: (response: ShopResponse) => {
        this.shopItems.set(response.vat_pham_list);
        this.tienVang.set(response.tien_vang_hien_tai);
        this.loading.set(false);
      },
      error: (err) => {
        console.error('Error loading shop:', err);
        this.loading.set(false);
        Swal.fire('Lỗi', 'Không thể tải cửa hàng', 'error');
      },
    });
  }

  purchaseItem(item: ShopItem): void {
    if (!item.co_the_mua) {
      if (item.thong_bao_gioi_han) {
        Swal.fire('Không thể mua', item.thong_bao_gioi_han, 'warning');
      } else {
        Swal.fire('Không đủ xu', `Bạn cần ${item.gia_xu} xu để mua vật phẩm này`, 'warning');
      }
      return;
    }

    Swal.fire({
      title: `Mua ${item.ten}?`,
      html: `
        <div class="purchase-confirm">
          <div class="item-icon" style="font-size: 3rem;">${item.icon}</div>
          <p class="item-name">${item.ten}</p>
          <p class="item-desc">${item.mo_ta}</p>
          <div class="price-info">
            <span class="price">💰 ${item.gia_xu} xu</span>
          </div>
          <p class="balance">Số dư: <strong>${this.tienVang()} xu</strong></p>
        </div>
      `,
      showCancelButton: true,
      confirmButtonText: '🛒 Mua ngay',
      cancelButtonText: 'Hủy',
      confirmButtonColor: '#10b981',
    }).then((result) => {
      if (result.isConfirmed) {
        this.executePurchase(item);
      }
    });
  }

  private executePurchase(item: ShopItem): void {
    this.purchasing.set(true);

    this.vatPhamService.purchaseItemById(item.id, 1).subscribe({
      next: (response: MuaVatPhamResponse) => {
        this.purchasing.set(false);

        if (response.thanh_cong) {
          // Update local gold
          if (response.tien_vang_con_lai !== undefined) {
            this.tienVang.set(response.tien_vang_con_lai);
          }

          // Update item purchasability
          this.updateItemAfterPurchase(item);

          Swal.fire({
            title: '🎉 Mua thành công!',
            html: `
              <p>${response.thong_bao}</p>
              <p>Số xu còn lại: <strong>${response.tien_vang_con_lai} xu</strong></p>
            `,
            icon: 'success',
            timer: 3000,
            showConfirmButton: false,
          });
        } else {
          Swal.fire('Không thể mua', response.thong_bao, 'error');
        }
      },
      error: (err) => {
        this.purchasing.set(false);
        console.error('Error purchasing item:', err);
        Swal.fire('Lỗi', 'Đã xảy ra lỗi khi mua vật phẩm', 'error');
      },
    });
  }

  private updateItemAfterPurchase(item: ShopItem): void {
    // Reload shop to get updated limits
    this.loadShop();
  }

  getRarityClass(rarity: string): string {
    return `rarity-${rarity.toLowerCase()}`;
  }

  getRarityLabel(rarity: string): string {
    switch (rarity) {
      case 'COMMON':
        return 'Thường';
      case 'RARE':
        return 'Hiếm';
      case 'EPIC':
        return 'Sử thi';
      case 'LEGENDARY':
        return 'Huyền thoại';
      default:
        return rarity;
    }
  }
}
