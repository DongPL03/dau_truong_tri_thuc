import { CommonModule } from '@angular/common';
import { Component, OnInit, inject, signal } from '@angular/core';
import { InventoryItem, KhoDoResponse } from '../../models/kho-do.model';
import { KhoDoService } from '../../services/kho-do.service';

@Component({
  selector: 'app-kho-do',
  standalone: true,
  imports: [CommonModule],
  templateUrl: './kho-do.html',
  styleUrl: './kho-do.scss',
})
export class KhoDo implements OnInit {
  private khoDoService = inject(KhoDoService);

  loading = signal(true);
  inventory = signal<KhoDoResponse | null>(null);
  selectedItem = signal<InventoryItem | null>(null);

  // Filter
  activeFilter = signal<string>('all');

  ngOnInit(): void {
    this.loadInventory();
  }

  loadInventory(): void {
    this.loading.set(true);
    this.khoDoService.getInventory().subscribe({
      next: (res) => {
        this.inventory.set(res);
        this.loading.set(false);
      },
      error: (err) => {
        console.error('Error loading inventory:', err);
        this.loading.set(false);
      },
    });
  }

  get filteredItems(): InventoryItem[] {
    const items = this.inventory()?.danh_sach_vat_pham || [];
    const filter = this.activeFilter();

    if (filter === 'all') return items;
    if (filter === 'usable') return items.filter((i) => i.co_the_su_dung);
    return items.filter((i) => i.do_hiem === filter);
  }

  setFilter(filter: string): void {
    this.activeFilter.set(filter);
  }

  selectItem(item: InventoryItem): void {
    this.selectedItem.set(item);
  }

  closeDetail(): void {
    this.selectedItem.set(null);
  }

  getRarityClass(doHiem: string): string {
    switch (doHiem) {
      case 'COMMON':
        return 'rarity-common';
      case 'RARE':
        return 'rarity-rare';
      case 'EPIC':
        return 'rarity-epic';
      case 'LEGENDARY':
        return 'rarity-legendary';
      default:
        return 'rarity-common';
    }
  }

  getRarityLabel(doHiem: string): string {
    switch (doHiem) {
      case 'COMMON':
        return 'Thường';
      case 'RARE':
        return 'Hiếm';
      case 'EPIC':
        return 'Sử thi';
      case 'LEGENDARY':
        return 'Huyền thoại';
      default:
        return 'Thường';
    }
  }
}
